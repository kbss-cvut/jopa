/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.TypeReferenceMap;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import cz.cvut.kbss.jopa.query.mapper.ResultSetMappingProcessor;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.Constants;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.HashSetValuedHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

public class MetamodelBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(MetamodelBuilder.class);

    private final NamedNativeQueryProcessor queryProcessor = new NamedNativeQueryProcessor();
    private final ResultSetMappingProcessor mappingProcessor;

    private final Map<Class<?>, AbstractIdentifiableType<?>> typeMap = new HashMap<>();
    private final Set<Class<?>> inferredClasses = new HashSet<>();

    private final MultiValuedMap<IdentifiableType<?>, AnnotatedAccessor> inheritableProperties = new HashSetValuedHashMap<>();
    private final TypeReferenceMap typeReferenceMap = new TypeReferenceMap();

    private final ConverterResolver converterResolver;

    private final Configuration configuration;

    public MetamodelBuilder(Configuration configuration) {
        this.configuration = configuration;
        this.mappingProcessor = new ResultSetMappingProcessor(this);
        this.converterResolver = new ConverterResolver(new Converters(configuration));
    }

    /**
     * Builds persistence unit metamodel based on classes discovered by the specified class finder.
     *
     * @param classFinder Holder of information about classes relevant for persistence unit building
     */
    public void buildMetamodel(PersistenceUnitClassFinder classFinder) {
        assert classFinder != null;
        classFinder.getAttributeConverters().forEach(converterResolver::registerConverter);
        classFinder.getEntities().forEach(this::processOWLClass);
        classFinder.getResultSetMappings().forEach(mappingProcessor::buildMapper);

    }

    /**
     * Builds persistence unit metamodel based on the specified entity classes.
     * <p>
     * No additional metamodel features (e.g., custom attribute converters, query result mappers) are built.
     *
     * @param entityClasses Entity classes to build metamodel for
     */
    public void buildMetamodel(Set<Class<?>> entityClasses) {
        assert entityClasses != null;
        entityClasses.forEach(this::processOWLClass);
    }

    private <X> void processOWLClass(final Class<X> cls) {
        if (typeMap.containsKey(cls)) {
            return;
        }

        LOG.debug("Processing OWL class: {}", cls);

        final TypeBuilderContext<X> et = ManagedClassProcessor.processManagedType(cls);
        et.setConverterResolver(converterResolver);
        et.setPuLanguage(configuration.get(JOPAPersistenceProperties.LANG));

        processManagedType(et);
    }


    private <X> void processMethods(Class<X> cls, AbstractIdentifiableType<X> type) {
        Arrays.stream(cls.getDeclaredMethods())
              .filter(MetamodelBuilder::isOWLPropertyMethod)
              .forEach(m -> inheritableProperties.put(type, AnnotatedAccessor.from(m)));
    }

    private static boolean isOWLPropertyMethod(Method m) {
        return m.getAnnotation(OWLDataProperty.class) != null ||
                m.getAnnotation(OWLAnnotationProperty.class) != null ||
                m.getAnnotation(OWLObjectProperty.class) != null;
    }

    private <X> void processManagedType(TypeBuilderContext<X> context) {
        final AbstractIdentifiableType<X> type = context.getType();
        final Class<X> cls = type.getJavaType();
        typeMap.put(cls, type);

        final Set<AbstractIdentifiableType<? super X>> supertypes = processSupertypes(cls);

        type.setSupertypes(supertypes);

        type.setLifecycleListenerManager(new EntityLifecycleCallbackResolver(type).resolve());

        final ClassFieldMetamodelProcessor<X> fieldProcessor = new ClassFieldMetamodelProcessor<>(context, this);

        for (Field f : cls.getDeclaredFields()) {
            fieldProcessor.processField(f);
        }

        processMethods(cls, type);

        if (!type.isAbstract()) {
            try {
                type.getIdentifier();
            } catch (IllegalArgumentException e) {
                throw new MetamodelInitializationException("Missing identifier field in entity " + cls);
            }
        }

        if (type.getPersistenceType() == Type.PersistenceType.ENTITY) {
            resolveInheritanceType((IdentifiableEntityType<X>) type);
        }

        queryProcessor.processClass(cls);
    }

    private <X> Set<AbstractIdentifiableType<? super X>> processSupertypes(Class<X> cls) {
        Set<AbstractIdentifiableType<? super X>> superTypes = new HashSet<>();

        final Set<Class<? super X>> managedSuperTypes = ManagedClassProcessor.getManagedSuperInterfaces(cls);

        final Class<? super X> managedSuperClass = ManagedClassProcessor.getManagedSuperClass(cls);

        if (managedSuperClass != null) {
            managedSuperTypes.add(managedSuperClass);
        }

        for (Class<? super X> managedSupertype : managedSuperTypes) {
            if (typeMap.containsKey(managedSupertype)) {
                superTypes.add((AbstractIdentifiableType<? super X>) typeMap.get(managedSupertype));
            } else {
                final TypeBuilderContext<? super X> context = ManagedClassProcessor.processManagedType(managedSupertype);
                context.setConverterResolver(converterResolver);
                context.setPuLanguage(configuration.get(JOPAPersistenceProperties.LANG));
                processManagedType(context);
                superTypes.add(context.getType());
            }
        }
        return superTypes;
    }

    private static <X> boolean canDeclareInheritanceStrategy(IdentifiableType<X> et) {
        return et.getSupertypes() == null || et.getSupertypes()
                                               .stream()
                                               .noneMatch(supertype -> supertype.getPersistenceType() == Type.PersistenceType.ENTITY);
    }

    private static <X> InheritanceType getInheritanceTypeFromParents(IdentifiableEntityType<X> et) {
        List<InheritanceType> superTypesInheritanceTypes = et.getSupertypes().stream()
                                                             .filter(superType -> superType.getPersistenceType() == Type.PersistenceType.ENTITY)
                                                             .map(abstractIdentifiableType -> ((IdentifiableEntityType<?>) abstractIdentifiableType).getInheritanceType())
                                                             .distinct()
                                                             .collect(Collectors.toList());
        if (superTypesInheritanceTypes.size() == 1) { /// there is an agreement from all parents on inheritance type
            return superTypesInheritanceTypes.get(0);
        } else {
            throw new MetamodelInitializationException("Entity " + et.getName() + " inherits two distinct inheritance strategies");
        }
    }

    private static <X> void resolveInheritanceType(IdentifiableEntityType<X> et) {
        final Class<X> cls = et.getJavaType();
        final Inheritance inheritance = cls.getDeclaredAnnotation(Inheritance.class);

        if (canDeclareInheritanceStrategy(et)) {
            if (inheritance != null) {
                et.setInheritanceType(inheritance.strategy());
            } else {
                et.setInheritanceType(Constants.DEFAULT_INHERITANCE_TYPE);
            }
        } else if (inheritance != null) {
            throw new MetamodelInitializationException("Class " + cls +
                                                               " cannot declare inheritance strategy, because it already inherits it from its supertype.");

        } else {
            et.setInheritanceType(getInheritanceTypeFromParents(et));
        }

    }

    public Map<Class<?>, ManagedType<?>> getTypeMap() {
        return Collections.unmodifiableMap(typeMap);
    }

    public <X> AbstractIdentifiableType<X> entity(Class<X> cls) {
        return (AbstractIdentifiableType<X>) typeMap.get(cls);
    }

    public Map<Class<?>, EntityType<?>> getEntities() {
        final Map<Class<?>, EntityType<?>> map = new HashMap<>();
        typeMap.entrySet().stream().filter(e -> e.getValue().getPersistenceType() == Type.PersistenceType.ENTITY)
               .forEach(e -> map.put(e.getKey(), (EntityType<?>) e.getValue()));
        return map;
    }

    public Set<Class<?>> getInferredClasses() {
        return Collections.unmodifiableSet(inferredClasses);
    }

    public NamedQueryManager getNamedQueryManager() {
        return queryProcessor.getQueryManager();
    }

    public ResultSetMappingManager getResultSetMappingManager() {
        return mappingProcessor.getManager();
    }

    void addInferredClass(Class<?> cls) {
        inferredClasses.add(cls);
    }

    @SuppressWarnings("unchecked")
    <X> ManagedType<X> getEntityClass(Class<X> cls) {
        if (!typeMap.containsKey(cls)) {
            processOWLClass(cls);
        }
        return (ManagedType<X>) typeMap.get(cls);
    }

    /**
     * Checks whether the specified class represents a managed type already processed by the metamodel builder.
     *
     * @param cls Class to check
     * @return Managed type existence status
     */
    boolean hasManagedType(Class<?> cls) {
        return typeMap.containsKey(cls);
    }

    void registerTypeReference(Class<?> referencedType, Class<?> referringType) {
        assert hasManagedType(referencedType);
        assert hasManagedType(referringType);
        typeReferenceMap.addReference(referencedType, referringType);
    }

    public TypeReferenceMap getTypeReferenceMap() {
        return typeReferenceMap;
    }

    public Collection<AnnotatedAccessor> getTypesPropertyMethods(IdentifiableType<?> k) {
        return inheritableProperties.get(k);
    }
}
