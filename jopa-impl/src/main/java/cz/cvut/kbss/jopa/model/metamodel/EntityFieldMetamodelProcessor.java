/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.*;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author ledvima1
 */
public class EntityFieldMetamodelProcessor<X> {

    private static final Logger LOG = Logger.getLogger(EntityFieldMetamodelProcessor.class.getName());

    private final FieldMappingValidator mappingValidator = new FieldMappingValidator();

    private final Class<X> cls;
    private final EntityTypeImpl<X> et;
    private final MetamodelImpl metamodel;

    public EntityFieldMetamodelProcessor(Class<X> cls, EntityTypeImpl<X> et, MetamodelImpl metamodel) {
        this.cls = cls;
        this.et = et;
        this.metamodel = metamodel;
    }

    public void processField(Field field) {
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("   processing field : " + field);
        }
        if (EntityPropertiesUtils.isFieldTransient(field)) {
            if (LOG.isLoggable(Level.FINE)) {
                LOG.fine("Skipping transient field " + field);
            }
            return;
        }
        if (field.getType().isPrimitive()) {
            throw new MetamodelInitializationException(
                    "Primitive types cannot be used for entity fields. Field " + field + " in class " + cls);
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

        final PropertyAttributes propertyAtt = PropertyAttributes.create(field);
        propertyAtt.resolve(field, metamodel, fieldValueCls);

        if (propertyAtt.isKnownOwlProperty()) {
            createAttribute(field, inference, propertyAtt);
        } else {
            final boolean success = processIdentifierField(field);
            if (!success) {
                throw new MetamodelInitializationException(
                        "Unable to process field " + field + ". It is not transient but has no mapping information.");
            }
        }
    }

    private Class<?> getFieldValueType(Field field) {
        if (Collection.class.isAssignableFrom(field.getType())) {
            return getSetOrListErasureType((ParameterizedType) field.getGenericType());
        } else if (field.getType().isArray()) {
            throw new MetamodelInitializationException("Array persistent attributes are not supported.");
        } else {
            return field.getType();
        }
    }

    private Class<?> getSetOrListErasureType(final ParameterizedType cls) {
        final Type[] t = cls.getActualTypeArguments();

        if (t.length != 1) {
            throw new OWLPersistenceException("Only collections with a single generic parameter are supported");
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
        mappingValidator.validateTypesField(field);
        et.addDirectTypes(new TypesSpecificationImpl<>(et, tt.fetchType(), field, fieldValueCls, inference.inferred));
    }

    private boolean isPropertiesField(Field field) {
        return field.getAnnotation(Properties.class) != null;
    }

    private void processPropertiesField(Field field, Class<?> fieldValueCls, InferenceInfo inference) {
        Properties properties = field.getAnnotation(Properties.class);
        mappingValidator.validatePropertiesField(field);
        et.addOtherProperties(new PropertiesSpecificationImpl<>(et, properties.fetchType(), field, fieldValueCls,
                inference.inferred));
    }

    private void createAttribute(Field field, InferenceInfo inference, PropertyAttributes propertyAttributes) {
        final Attribute<X, ?> a;
        if (field.getType().isAssignableFrom(List.class)) {
            final Sequence os = field.getAnnotation(Sequence.class);

            if (os == null) {
                throw new MetamodelInitializationException("Expected Sequence annotation.");
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
                                 .participationConstraints(propertyAttributes.getParticipationConstraints()).build();
        } else if (field.getType().isAssignableFrom(Set.class)) {
            a = SetAttributeImpl.iri(propertyAttributes.getIri()).declaringType(et).field(field)
                                .elementType(propertyAttributes.getType())
                                .attributeType(propertyAttributes.getPersistentAttributeType())
                                .fetchType(propertyAttributes.getFetchType())
                                .cascadeTypes(propertyAttributes.getCascadeTypes()).inferred(inference.inferred)
                                .includeExplicit(inference.includeExplicit)
                                .participationConstraints(propertyAttributes.getParticipationConstraints()).build();
        } else if (field.getType().isAssignableFrom(Map.class)) {
            throw new IllegalArgumentException("NOT YET SUPPORTED");
        } else {
            a = SingularAttributeImpl.iri(propertyAttributes.getIri()).name(field.getName()).identifier(false)
                                     .declaringType(et).type(propertyAttributes.getType()).field(field)
                                     .cascadeTypes(propertyAttributes.getCascadeTypes())
                                     .attributeType(propertyAttributes.getPersistentAttributeType())
                                     .fetchType(propertyAttributes.getFetchType()).inferred(inference.inferred)
                                     .includeExplicit(inference.includeExplicit)
                                     .constraints(propertyAttributes.getParticipationConstraints())
                                     .nonEmpty(propertyAttributes.isNonEmpty()).build();
        }
        et.addDeclaredAttribute(field.getName(), a);
    }

    private boolean processIdentifierField(Field field) {
        final Id id = field.getAnnotation(Id.class);
        if (id == null) {
            return false;
        }
        if (!mappingValidator.isValidIdentifierType(field.getType())) {
            throw new IllegalArgumentException("NOT YET SUPPORTED");
        }
        et.setIdentifier(new IRIIdentifierImpl(field, id.generated()));
        return true;
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
