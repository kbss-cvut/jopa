package cz.cvut.kbss.jopa.owlapi.metamodel;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.owlapi.*;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author ledvima1
 */
public class EntityFieldMetamodelProcessor<X> {

    private static final Logger LOG = Logger.getLogger(EntityFieldMetamodelProcessor.class.getName());

    private static Collection<Class<?>> VALID_ID_CLASSES = Arrays.asList(new Class<?>[]{String.class, URI.class});

    private Class<X> cls;
    private EntityTypeImpl<X> et;
    private MetamodelImpl metamodel;

    public EntityFieldMetamodelProcessor(Class<X> cls, EntityTypeImpl<X> et, MetamodelImpl metamodel) {
        this.cls = cls;
        this.et = et;
        this.metamodel = metamodel;
    }

    public void processField(Field field) {
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("   processing field : " + field);
        }
        if (field.getType().isPrimitive()) {
            throw new OWLPersistenceException("Primitive types cannot be used for field types");
        }

        final Class<?> fieldValueCls = getFieldValueType(field);
        field.setAccessible(true);
        final InferenceInfo inference = processInferenceInfo(field);

        if (isTypesField(field)) {
            processTypesField(field, fieldValueCls, inference);
            return;
        }
        if (isPropertiesField(field)) {
            processPropertiesField(field, fieldValueCls, inference);
            return;
        }

        final ParticipationConstraint[] ics = getParticipationConstraints(field);

        final PropertyAttributes propertyAtt = PropertyAttributes.create(field);
        propertyAtt.resolve(field, metamodel, fieldValueCls);

        if (propertyAtt.isKnownOwlProperty()) {
            createAttribute(field, inference, ics, propertyAtt);
        } else {
            processIdentifierField(field);
        }
    }

    private Class<?> getFieldValueType(Field field) {
        if (Collection.class.isAssignableFrom(field.getType())) {
            return getSetOrListErasureType((ParameterizedType) field.getGenericType());
        } else {
            return field.getType();
        }
    }

    private Class<?> getSetOrListErasureType(final ParameterizedType cls) {
        final Type[] t = cls.getActualTypeArguments();

        if (t.length != 1) {
            throw new OWLPersistenceException(
                    "Only valid OWLClass annotated classes can be used as parameters for lists and sets.");
        }

        Type type = t[0];

        if (!(type instanceof Class<?>)) {
            throw new OWLPersistenceException("Only Classes might be valid parameters for generic lists and sets");
        }

        return (Class<?>) type;
    }

    private InferenceInfo processInferenceInfo(Field field) {
        final Inferred inferred = field.getAnnotation(Inferred.class);

        final InferenceInfo inference = new InferenceInfo(inferred);
        if (inference.inferred) {
            metamodel.addInferredClass(cls);
        }
        return inference;
    }

    private boolean isTypesField(Field field) {
        return field.getAnnotation(Types.class) != null;
    }

    private void processTypesField(Field field, Class<?> fieldValueCls, InferenceInfo inference) {
        Types tt = field.getAnnotation(Types.class);
        if (!Set.class.isAssignableFrom(field.getType())) {
            throw new OWLPersistenceException("The Types element must be a set of Strings.");
        }
        et.addDirectTypes(new TypesSpecificationImpl(et,
                tt.fetchType(), field, fieldValueCls, inference.inferred));
    }

    private boolean isPropertiesField(Field field) {
        return field.getAnnotation(Properties.class) != null;
    }

    private void processPropertiesField(Field field, Class<?> fieldValueCls, InferenceInfo inference) {
        Properties properties = field.getAnnotation(Properties.class);
        if (!Map.class.isAssignableFrom(field.getType())) {
            throw new OWLPersistenceException("The Types element must be a Map<String,Set<String>>.");
        }
        et.addOtherProperties(new PropertiesSpecificationImpl(et,
                properties.fetchType(), field, fieldValueCls, inference.inferred));
    }

    private ParticipationConstraint[] getParticipationConstraints(Field field) {
        final ParticipationConstraint[] ics;
        ParticipationConstraints cons = field.getAnnotation(ParticipationConstraints.class);
        if (cons != null && cons.value() != null) {
            ics = cons.value();
        } else {
            ics = new ParticipationConstraint[]{};
        }
        return ics;
    }

    private void createAttribute(Field field, InferenceInfo inference, ParticipationConstraint[] ics,
                                 PropertyAttributes propertyAttributes) {
        final Attribute<X, ?> a;
        if (field.getType().isAssignableFrom(List.class)) {
            final Sequence os = field.getAnnotation(Sequence.class);

            if (os == null) {
                throw new OWLPersistenceException("Expected Sequence annotation.");
            }
            a = ListAttributeImpl.iri(propertyAttributes.getIri()).declaringType(et).field(field)
                                 .elementType(propertyAttributes.getType())
                                 .attributeType(propertyAttributes.getPersistentAttributeType())
                                 .cascadeTypes(propertyAttributes.getCascadeTypes())
                                 .fetchType(propertyAttributes.getFetchType()).inferred(inference.inferred)
                                 .includeExplicit(inference.includeExplicit)
                                 .owlListClass(IRI.create(os.ClassOWLListIRI()))
                                 .hasNextProperty(IRI.create(os.ObjectPropertyHasNextIRI()))
                                 .hasContentsProperty(IRI.create(os.ObjectPropertyHasContentsIRI()))
                                 .sequenceType(os.type())
                                 .participationConstraints(ics).build();
        } else if (field.getType().isAssignableFrom(Set.class)) {
            a = SetAttributeImpl.iri(propertyAttributes.getIri()).declaringType(et).field(field)
                                .elementType(propertyAttributes.getType())
                                .attributeType(propertyAttributes.getPersistentAttributeType())
                                .fetchType(propertyAttributes.getFetchType())
                                .cascadeTypes(propertyAttributes.getCascadeTypes()).inferred(inference.inferred)
                                .includeExplicit(inference.includeExplicit).participationConstraints(ics).build();
        } else if (field.getType().isAssignableFrom(Map.class)) {
            throw new IllegalArgumentException("NOT YET SUPPORTED");
        } else {
            a = SingularAttributeImpl.iri(propertyAttributes.getIri()).name(field.getName()).identifier(false)
                                     .declaringType(et).type(propertyAttributes.getType()).field(field)
                                     .cascadeTypes(propertyAttributes.getCascadeTypes())
                                     .attributeType(propertyAttributes.getPersistentAttributeType())
                                     .fetchType(propertyAttributes.getFetchType())
                                     .inferred(inference.inferred).includeExplicit(inference.includeExplicit)
                                     .constraints(ics).build();
        }
        et.addDeclaredAttribute(field.getName(), a);
    }

    private void processIdentifierField(Field field) {
        final Id id = field.getAnnotation(Id.class);
        if (id == null) {
            return;
        }
        if (!VALID_ID_CLASSES.contains(field.getType())) {
            throw new IllegalArgumentException("NOT YET SUPPORTED");
        }
        et.setIdentifier(new IRIIdentifierImpl(field, id.generated()));
    }

    private static class InferenceInfo {
        private final boolean inferred;
        private final boolean includeExplicit;

        public InferenceInfo(Inferred inferredAnnotation) {
            this.inferred = inferredAnnotation != null;
            this.includeExplicit = inferredAnnotation == null || inferredAnnotation.includeExplicit();
        }
    }
}
