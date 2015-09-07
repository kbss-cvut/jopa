/**
 * Copyright (C) 2011 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.owlapi;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class MetamodelImpl implements Metamodel {

    private static final Logger LOG = Logger.getLogger(Metamodel.class
            .getName());

    private static final String ASPECTJ_CLASS = "org.aspectj.weaver.loadtime.Agent";

    private static Collection<Class<?>> validIdClasses = Arrays.asList(new Class<?>[]{String.class, URI.class});

    private final Map<Class<?>, EntityType<?>> typeMap = new HashMap<>();

    private final Set<Class<?>> inferredClasses = new HashSet<>();

    private EntityManagerFactoryImpl emf;

    private Set<URI> moduleExtractionSignature;

    private final Set<Class<?>> entities = new HashSet<>();

    MetamodelImpl(final EntityManagerFactoryImpl emf, EntityLoader entityLoader) {
        this.emf = emf;
        build(entityLoader);
    }

    private void build(EntityLoader entityLoader) {
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("Building metamodel ... ");
        }
        checkForWeaver();

        loadEntities(entityLoader);

        entities.forEach((cls) -> processOWLClass(cls));
    }

    /**
     * Check the class path for aspectj weaver, which is vital for using lazy loading.
     */
    private void checkForWeaver() {
        try {
            @SuppressWarnings("unused")
            Class<?> c = MetamodelImpl.class.getClassLoader().loadClass(
                    ASPECTJ_CLASS);
        } catch (ClassNotFoundException e) {
            LOG.severe("AspectJ not found on classpath. Cannot run without AspectJ.");
            throw new OWLPersistenceException(e);
        }
    }

    private void loadEntities(EntityLoader entityLoader) {
        Set<Class<?>> discoveredEntities = entityLoader.discoverEntityClasses(emf.getProperties());
        entities.addAll(discoveredEntities);
    }

    <X> void processOWLClass(final Class<X> cls) {
        // TODO Refactor this method
        // TODO Create tests for MetamodelImpl, especially this method
        if (typeMap.containsKey(cls)) {
            return;
        }

        if (LOG.isLoggable(Level.CONFIG)) {
            LOG.config("processing OWL class : " + cls);
        }

        final OWLClass c = cls.getAnnotation(OWLClass.class);

        if (c == null) {
            throw new OWLPersistenceException("The class " + cls
                    + " is not an OWLPersistence entity!");
        }

        final EntityTypeImpl<X> c2 = new EntityTypeImpl<>(cls.getSimpleName(),
                cls, IRI.create(c.iri()));

        typeMap.put(cls, c2);

        for (final Field field : cls.getDeclaredFields()) {
            if (LOG.isLoggable(Level.FINE)) {
                LOG.fine("   processing field : " + field);
            }

            if (field.getType().isPrimitive()) {
                throw new OWLPersistenceException(
                        "Primitive types cannot be used for field types");
            }

            final Class<?> cxx;

            if (Collection.class.isAssignableFrom(field.getType())) {
                cxx = getSetOrListErasureType((ParameterizedType) field
                        .getGenericType());
            } else {
                cxx = field.getType();
            }
            field.setAccessible(true);
            final Inferred inferred = field.getAnnotation(Inferred.class);
            final boolean isInferred = inferred != null;
            final boolean includeExplicit = inferred == null || inferred.includeExplicit();
            if (isInferred) {
                inferredClasses.add(cls);
            }

            Types tt = field.getAnnotation(Types.class);
            if (tt != null) {
                if (!Set.class.isAssignableFrom(field.getType())) {
                    throw new OWLPersistenceException(
                            "The Types element must be a set of Strings.");
                }
                c2.addDirectTypes(new TypesSpecificationImpl(c2,
                        tt.fetchType(), field, cxx, isInferred));
                continue;
            }

            cz.cvut.kbss.jopa.model.annotations.Properties properties = field
                    .getAnnotation(cz.cvut.kbss.jopa.model.annotations.Properties.class);
            if (properties != null) {
                if (!Map.class.isAssignableFrom(field.getType())) {
                    throw new OWLPersistenceException(
                            "The Types element must be a Map<String,Set<String>>.");
                }
                c2.addOtherProperties(new PropertiesSpecificationImpl(c2,
                        properties.fetchType(), field, cxx, isInferred));
                continue;
            }

            OWLObjectProperty oop = field
                    .getAnnotation(OWLObjectProperty.class);
            OWLDataProperty odp = field.getAnnotation(OWLDataProperty.class);
            OWLAnnotationProperty oap = field
                    .getAnnotation(OWLAnnotationProperty.class);

            cz.cvut.kbss.jopa.model.metamodel.Type<?> type = null;
            PersistentAttributeType t = null;
            IRI iri = null;
            CascadeType[] cascadeTypes = new CascadeType[]{};
            // TODO Figure out some strategy for blobs etc., they can't be loaded eagerly
            FetchType fetchType = FetchType.EAGER;

            ParticipationConstraint[] ics = null;
            ParticipationConstraints cons = field
                    .getAnnotation(ParticipationConstraints.class);
            if (cons != null) {
                if (cons.value() != null) {
                    ics = cons.value();
                }
            }

            if (ics == null) {
                ics = new ParticipationConstraint[]{};
            }

            if (oop != null) {
                if (LOG.isLoggable(Level.FINE)) {
                    LOG.fine("     Object property: " + oop);
                }
                t = PersistentAttributeType.OBJECT;
                iri = IRI.create(oop.iri());
                cascadeTypes = oop.cascade();
                fetchType = oop.fetch();
                processOWLClass(cxx);
                type = typeMap.get(cxx);
            } else if (odp != null) {
                if (LOG.isLoggable(Level.FINE)) {
                    LOG.fine("     Data property: " + odp);
                }
                t = PersistentAttributeType.DATA;
                iri = IRI.create(odp.iri());
                type = BasicTypeImpl.get(cxx);
            } else if (oap != null) {
                if (LOG.isLoggable(Level.FINE)) {
                    LOG.fine("     Annotation property: " + oap);
                }
                t = PersistentAttributeType.ANNOTATION;
                iri = IRI.create(oap.iri());
                type = BasicTypeImpl.get(cxx);
            }

            final Attribute<X, ?> a;

            if (t != null) {
                if (field.getType().isAssignableFrom(List.class)) {
                    final Sequence os = field.getAnnotation(Sequence.class);

                    if (os == null) {
                        throw new OWLPersistenceException(
                                "Expected OWLSequence annotation.");
                    }

                    a = ListAttributeImpl.iri(iri).declaringType(c2).field(field).elementType(type).attributeType(t)
                                         .cascadeTypes(cascadeTypes).fetchType(fetchType).inferred(isInferred)
                                         .includeExplicit(includeExplicit)
                                         .owlListClass(IRI.create(os.ClassOWLListIRI()))
                                         .hasNextProperty(IRI.create(os.ObjectPropertyHasNextIRI()))
                                         .hasContentsProperty(IRI.create(os.ObjectPropertyHasContentsIRI()))
                                         .sequenceType(os.type())
                                         .participationConstraints(ics).build();
                } else if (field.getType().isAssignableFrom(Set.class)) {
                    if (oop != null) {
                        processOWLClass(cxx);
                    }
                    a = SetAttributeImpl.iri(iri).declaringType(c2).field(field).elementType(type).attributeType(t)
                                        .fetchType(fetchType).cascadeTypes(cascadeTypes).inferred(isInferred)
                                        .includeExplicit(includeExplicit)
                                        .participationConstraints(ics).build();
                } else if (field.getType().isAssignableFrom(Map.class)) {
                    throw new IllegalArgumentException("NOT YET SUPPORTED");
                } else {
                    a = SingularAttributeImpl.iri(iri).name(field.getName()).identifier(false).declaringType(c2)
                                             .type(type)
                                             .field(field).cascadeTypes(cascadeTypes).attributeType(t)
                                             .fetchType(fetchType).inferred(isInferred)
                                             .includeExplicit(includeExplicit).constraints(ics).build();
                }
                c2.addDeclaredAttribute(field.getName(), a);
            } else {
                final Id id = field.getAnnotation(Id.class);

                if (id == null) {
                    continue;
                }

                if (!validIdClasses.contains(field.getType())) {
                    throw new IllegalArgumentException("NOT YET SUPPORTED");
                }
                c2.setIdentifier(new IRIIdentifierImpl(field, id.generated()));
            }
        }

        if (c2.getIdentifier() == null) {
            throw new IllegalArgumentException();
        }

        // try {
        // if (cls.isEnum()) {
        // for (final Object e : cls.getEnumConstants()) {
        // final Field idField = getId(e.getClass());
        // IRI id = IRI.create((URI) idField.get(e));
        //
        // if (id == null) {
        // throw new OWLPersistenceException(
        // "An enum individual must have an id specified");
        // }
        //
        // final OWLNamedIndividual i = m.getOWLDataFactory()
        // .getOWLNamedIndividual(id);
        //
        // enumEntities.put(i, e);
        // instanceCache.put(e, i);
        // }
        // }
        // } catch (IllegalArgumentException e1) {
        // throw new OWLPersistenceException(e1);
        // } catch (IllegalAccessException e1) {
        // throw new OWLPersistenceException(e1);
        // }
    }

    private Class<?> getSetOrListErasureType(final ParameterizedType cls) {
        final Type[] t = cls.getActualTypeArguments();

        if (t.length != 1) {
            throw new OWLPersistenceException(
                    "Only valid OWLClass annotated classes can be used as parameters for lists and sets.");
        }

        Type type = t[0];

        if (!(type instanceof Class<?>)) {
            throw new OWLPersistenceException(
                    "Only Classes might be valid parameters for generic lists and sets");
        }

        return (Class<?>) type;
    }

    @SuppressWarnings("unchecked")
    public <X> EntityType<X> entity(Class<X> cls) {
        if (!typeMap.containsKey(cls)) {
            // TODO
            processOWLClass(cls);
        }

        return (EntityType<X>) typeMap.get(cls);
    }

    public <X> EmbeddableType<X> embeddable(Class<X> cls) {
        throw new IllegalArgumentException("Embeddable entities not supported.");
    }

    public Set<EmbeddableType<?>> getEmbeddables() {
        return Collections.emptySet();
    }

    public Set<EntityType<?>> getEntities() {
        return new HashSet<>(typeMap.values());
    }

    public Set<ManagedType<?>> getManagedTypes() {
        return new HashSet<>(typeMap.values());
    }

    public <X> ManagedType<X> managedType(Class<X> cls) {
        return entity(cls);
    }

    public Set<Class<?>> getInferredClasses() {
        return this.inferredClasses;
    }

    @Override
    public Set<URI> getModuleExtractionExtraSignature() {
        return Collections.unmodifiableSet(getSignatureInternal());
    }

    @Override
    public void addUriToModuleExtractionSignature(URI uri) {
        if (uri == null) {
            throw new NullPointerException();
        }
        synchronized (this) {
            getSignatureInternal().add(uri);
        }
    }

    private synchronized Set<URI> getSignatureInternal() {
        // This can be lazily loaded since we don't know if we'll need it
        if (moduleExtractionSignature == null) {
            final String sig = emf.getProperties().get(OntoDriverProperties.MODULE_EXTRACTION_SIGNATURE);
            if (sig == null) {
                this.moduleExtractionSignature = new HashSet<>();
            } else {
                final String[] signature = sig.split(OntoDriverProperties.SIGNATURE_DELIMITER);
                this.moduleExtractionSignature = new HashSet<>(signature.length);
                try {
                    for (String uri : signature) {
                        moduleExtractionSignature.add(new URI(uri));
                    }
                } catch (URISyntaxException e) {
                    throw new OWLPersistenceException("Invalid URI encountered in module extraction signature.", e);
                }
            }
        }
        return moduleExtractionSignature;
    }
}
