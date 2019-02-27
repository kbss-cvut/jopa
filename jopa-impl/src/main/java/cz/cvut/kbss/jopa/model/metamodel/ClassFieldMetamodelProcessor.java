/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.BeanListenerAspect;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

class ClassFieldMetamodelProcessor<X> {

    private static final Logger LOG = LoggerFactory.getLogger(ClassFieldMetamodelProcessor.class);

    private final FieldMappingValidator mappingValidator = new FieldMappingValidator();

    private final Class<X> cls;
    private final AbstractIdentifiableType<X> et;
    private final TypeBuilderContext<X> context;
    private final MetamodelBuilder metamodelBuilder;

    ClassFieldMetamodelProcessor(TypeBuilderContext<X> context, MetamodelBuilder metamodelBuilder) {
        this.cls = context.getType().getJavaType();
        this.context = context;
        this.et = context.getType();
        this.metamodelBuilder = metamodelBuilder;
    }

    void processField(Field field) {
        LOG.trace("processing field: {}", field);
        if (EntityPropertiesUtils.isFieldTransient(field)) {
            // Do not log static fields
            if (!EntityPropertiesUtils.isFieldStatic(field)) {
                LOG.trace("Skipping transient field {}", field);
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

        final PropertyAttributes propertyAtt = PropertyAttributes.create(field, mappingValidator, context);
        propertyAtt.resolve(field, metamodelBuilder, fieldValueCls);

        if (propertyAtt.isKnownOwlProperty()) {
            createAttribute(field, inference, propertyAtt);
        } else if (!isAspectIntegrationField(field)) {
            final boolean success = processIdentifierField(field);
            if (!success) {
                throw new MetamodelInitializationException(
                        "Unable to process field " + field + ". It is not transient but has no mapping information.");
            }
        }
    }

    private static Class<?> getFieldValueType(Field field) {
        if (Collection.class.isAssignableFrom(field.getType())) {
            return getSetOrListErasureType((ParameterizedType) field.getGenericType());
        } else if (field.getType().isArray()) {
            throw new MetamodelInitializationException("Array persistent attributes are not supported.");
        } else {
            return field.getType();
        }
    }

    private static Class<?> getSetOrListErasureType(final ParameterizedType cls) {
        final Type[] t = cls.getActualTypeArguments();

        if (t.length != 1) {
            throw new OWLPersistenceException("Only collections with a single generic parameter are supported.");
        }
        Type type = t[0];
        if (!(type instanceof Class<?>)) {
            throw new OWLPersistenceException("Only Classes are valid parameters for generic lists and sets.");
        }
        return (Class<?>) type;
    }

    private InferenceInfo processInferenceInfo(Field field) {
        final Inferred inferred = field.getAnnotation(Inferred.class);

        final InferenceInfo inference = new InferenceInfo(inferred);
        if (inference.inferred) {
            metamodelBuilder.addInferredClass(cls);
        }
        return inference;
    }

    private static boolean isTypesField(Field field) {
        return field.getAnnotation(Types.class) != null;
    }

    private void processTypesField(Field field, Class<?> fieldValueCls, InferenceInfo inference) {
        Types tt = field.getAnnotation(Types.class);
        mappingValidator.validateTypesField(field);
        et.addDirectTypes(new TypesSpecificationImpl<>(et, tt.fetchType(), field, fieldValueCls, inference.inferred));
    }

    private static boolean isPropertiesField(Field field) {
        return field.getAnnotation(Properties.class) != null;
    }

    private void processPropertiesField(Field field, Class<?> fieldValueCls, InferenceInfo inference) {
        Properties properties = field.getAnnotation(Properties.class);
        mappingValidator.validatePropertiesField(field);
        final PropertiesParametersResolver paramsResolver = new PropertiesParametersResolver(field);
        et.addOtherProperties(
                PropertiesSpecificationImpl.declaringType(et).fetchType(properties.fetchType()).javaField(field)
                                           .javaType(fieldValueCls).inferred(inference.inferred)
                                           .propertyIdType(paramsResolver.getPropertyIdentifierType())
                                           .propertyValueType(paramsResolver.getPropertyValueType()).build());
    }

    private void createAttribute(Field field, InferenceInfo inference, PropertyAttributes propertyAttributes) {
        final AbstractAttribute<X, ?> a;
        if (field.getType().isAssignableFrom(List.class)) {
            final Sequence os = field.getAnnotation(Sequence.class);

            if (os == null) {
                throw new MetamodelInitializationException("Expected Sequence annotation.");
            }
            final ListAttributeImpl.ListAttributeBuilder builder =
                    ListAttributeImpl.iri(propertyAttributes.getIri()).declaringType(et).field(field)
                                     .elementType(propertyAttributes.getType())
                                     .attributeType(propertyAttributes.getPersistentAttributeType())
                                     .cascadeTypes(propertyAttributes.getCascadeTypes())
                                     .fetchType(propertyAttributes.getFetchType()).inferred(inference.inferred)
                                     .includeExplicit(inference.includeExplicit)
                                     .owlListClass(IRI.create(os.ClassOWLListIRI()))
                                     .hasNextProperty(IRI.create(os.ObjectPropertyHasNextIRI()))
                                     .hasContentsProperty(IRI.create(os.ObjectPropertyHasContentsIRI()))
                                     .sequenceType(os.type())
                                     .participationConstraints(propertyAttributes.getParticipationConstraints())
                                     .nonEmpty(propertyAttributes.isNonEmpty());
            context.getConverterResolver().resolveConverter(field, propertyAttributes).ifPresent(builder::converter);
            a = builder.build();
        } else if (field.getType().isAssignableFrom(Set.class)) {
            final AbstractPluralAttribute.PluralAttributeBuilder builder =
                    SetAttributeImpl.iri(propertyAttributes.getIri()).declaringType(et)
                                    .field(field)
                                    .elementType(propertyAttributes.getType())
                                    .attributeType(propertyAttributes.getPersistentAttributeType())
                                    .fetchType(propertyAttributes.getFetchType())
                                    .cascadeTypes(propertyAttributes.getCascadeTypes())
                                    .inferred(inference.inferred)
                                    .includeExplicit(inference.includeExplicit)
                                    .participationConstraints(propertyAttributes.getParticipationConstraints())
                                    .nonEmpty(propertyAttributes.isNonEmpty());
            context.getConverterResolver().resolveConverter(field, propertyAttributes).ifPresent(builder::converter);
            a = (AbstractAttribute<X, ?>) builder.build();
        } else if (field.getType().isAssignableFrom(Map.class)) {
            throw new IllegalArgumentException("NOT YET SUPPORTED");
        } else {
            final SingularAttributeImpl.SingularAttributeBuilder builder =
                    SingularAttributeImpl.iri(propertyAttributes.getIri()).declaringType(et)
                                         .type(propertyAttributes.getType()).field(field)
                                         .cascadeTypes(propertyAttributes.getCascadeTypes())
                                         .attributeType(propertyAttributes.getPersistentAttributeType())
                                         .fetchType(propertyAttributes.getFetchType())
                                         .inferred(inference.inferred)
                                         .includeExplicit(inference.includeExplicit)
                                         .constraints(propertyAttributes.getParticipationConstraints())
                                         .nonEmpty(propertyAttributes.isNonEmpty());
            context.getConverterResolver().resolveConverter(field, propertyAttributes).ifPresent(builder::converter);
            a = (AbstractAttribute<X, ?>) builder.build();
        }
        et.addDeclaredAttribute(field.getName(), a);
    }

    private boolean processIdentifierField(Field field) {
        final Id id = field.getAnnotation(Id.class);
        if (id == null) {
            return false;
        }
        mappingValidator.validateIdentifierType(field.getType());
        et.setIdentifier(new IRIIdentifierImpl<>(et, field, id.generated()));
        return true;
    }

    private static boolean isAspectIntegrationField(Field field) {
        // AspectJ integration fields cannot be declared transitive (they're generated by the AJC), so we have to
        // skip them manually
        return field.getType().equals(BeanListenerAspect.Manageable.class);
    }

    private static class InferenceInfo {
        private final boolean inferred;
        private final boolean includeExplicit;

        InferenceInfo(Inferred inferredAnnotation) {
            this.inferred = inferredAnnotation != null;
            this.includeExplicit = inferredAnnotation == null || inferredAnnotation.includeExplicit();
        }
    }
}
