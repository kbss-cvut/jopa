/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
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
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;

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
        final InferenceInfo inference = processInferenceInfo(field); // todo - is missing in property method

        if (isTypesField(field)) {
            processTypesField(field, fieldValueCls, inference);
            return;
        }
        if (isPropertiesField(field)) {
            processPropertiesField(field, fieldValueCls, inference);
            return;
        }

        if (isQueryAttribute(field)) {
            createQueryAttribute(field, fieldValueCls);
            return;
        }

        PropertyInfo propertyInfo = PropertyInfo.from(field);

        final PropertyAttributes propertyAtt = PropertyAttributes.create(propertyInfo, mappingValidator, context);
        propertyAtt.resolve(propertyInfo, metamodelBuilder, fieldValueCls);

        if (propertyAtt.isKnownOwlProperty()) {
            final AbstractAttribute<X, ?> a = createAttribute(propertyInfo, inference, propertyAtt);
            registerTypeReference(a);
            return;
        }

        if (isAspectIntegrationField(field)) {
            return;
        }

        if (tryProcessIdentifierField(field)) {
            return;
        }
        LOG.error("finding method to {} ", field);
        findPropertyDefinitionInHierarchy(field, inference);
    }

    private void findPropertyDefinitionInHierarchy(Field field, InferenceInfo inference) {

        Set<IdentifiableType<?>> stack = new HashSet<>(et.getSupertypes());
        boolean found = false;
        Method foundMethod = null;
        while (!stack.isEmpty()) {
            IdentifiableType<?> top = stack.iterator().next();
            Collection<AnnotatedAccessor> annotatedAccessors = metamodelBuilder.getTypesPropertyMethods(top);

            for (AnnotatedAccessor annotatedAccessor : annotatedAccessors) {
                if (propertyBelongsToMethod(field, annotatedAccessor)) {
                    LOG.error("Found belonging - {} - {}", field.getName(), annotatedAccessor.getMethod().getName());

                    if (found && !annotatedMethodsEqual(annotatedAccessor.getMethod(), foundMethod)) { ///annotatedAccessor.getMethod().equals(foundMethod)
                        throw new MetamodelInitializationException(
                                "Ambiguous hierarchy - fields can inherit only from multiple methods if their property mapping annotations equal. However for field "
                                        + field + " two non-compatible methods were found - " + foundMethod + " and " + annotatedAccessor.getMethod());
                    }
                    final PropertyInfo info = new PropertyInfo.MethodInfo(annotatedAccessor.getMethod(), field);

                    final PropertyAttributes propertyAtt = PropertyAttributes.create(info, mappingValidator, context);
                    propertyAtt.resolve(info, metamodelBuilder, field.getType());

                    final AbstractAttribute<X, ?> a = createAttribute(info, inference, propertyAtt);
                    registerTypeReference(a);

                    found = true;
                    foundMethod = annotatedAccessor.getMethod();
                }
            }
            stack.addAll(top.getSupertypes());
            stack.remove(top);
        }
        if (!found) {
            throw new MetamodelInitializationException(
                    "Unable to process field " + field + ". It is not transient but has no mapping information.");
        }
    }

    private boolean annotatedMethodsEqual(Method newMethod, Method foundMethod) {

        if (foundMethod.getAnnotation(OWLObjectProperty.class) != null) {
            return foundMethod.getAnnotation(OWLObjectProperty.class).equals(newMethod.getAnnotation(OWLObjectProperty.class));
        } else if (foundMethod.getAnnotation(OWLDataProperty.class) != null) {
            return foundMethod.getAnnotation(OWLDataProperty.class).equals(newMethod.getAnnotation(OWLDataProperty.class));
        } else {
            return foundMethod.getAnnotation(OWLAnnotationProperty.class) != null && foundMethod.getAnnotation(OWLAnnotationProperty.class).equals(newMethod.getAnnotation(OWLAnnotationProperty.class));
        }
    }

    private boolean methodInfo(String propertyName, String methodName, Class<?> cls) { // todo
        try {
            BeanInfo beanInfo = Introspector.getBeanInfo(cls);
            PropertyDescriptor[] props = beanInfo.getPropertyDescriptors();
            for (PropertyDescriptor pd : props) {

                LOG.error("-----");
                LOG.error(pd.getName());
                if (pd.getReadMethod() != null) {
                    LOG.error(pd.getReadMethod().getName());
                }
                if (pd.getWriteMethod() != null) {

                    LOG.error(pd.getWriteMethod().getName());
                }
            }
        } catch (Exception e) {
            LOG.error("exc");
        }
        return false;
    }

    /// todo if not assignable, throw exception ?
    private boolean propertyBelongsToMethod(Field property, AnnotatedAccessor accessor) {

        return property.getName().equals(accessor.getPropertyName()) && property.getType()
                .isAssignableFrom(accessor.getPropertyType());
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

    private static boolean isQueryAttribute(Field field) {
        return field.getAnnotation(Sparql.class) != null;
    }

    private void createQueryAttribute(Field field, Class<?> fieldValueCls) {
        final Sparql sparqlAnnotation = field.getAnnotation(Sparql.class);
        final String query = sparqlAnnotation.query();
        final FetchType fetchType = sparqlAnnotation.fetchType();

        ParticipationConstraint[] participationConstraints = field.getAnnotationsByType(ParticipationConstraint.class);

        final AbstractQueryAttribute<X, ?> a;

        cz.cvut.kbss.jopa.model.metamodel.Type<?> type;

        if (ManagedClassProcessor.isManagedType(fieldValueCls)) {
            type = metamodelBuilder.getEntityClass(fieldValueCls);
        } else {
            type = BasicTypeImpl.get(fieldValueCls);
        }

        Optional<ConverterWrapper<?, ?>> optionalConverterWrapper = context.getConverterResolver()
                .resolveConverter(type);
        ConverterWrapper<?, ?> converterWrapper = null;

        if (optionalConverterWrapper.isPresent()) {
            converterWrapper = optionalConverterWrapper.get();
        }

        if (Collection.class.isAssignableFrom(field.getType())) {
            a = new PluralQueryAttributeImpl<>(query, sparqlAnnotation.enableReferencingAttributes(), field, et, fetchType, participationConstraints, type, field.getType(), converterWrapper);
        } else if (Map.class.isAssignableFrom(field.getType())) {
            throw new IllegalArgumentException("NOT YET SUPPORTED");
        } else {
            a = new SingularQueryAttributeImpl<>(query, sparqlAnnotation.enableReferencingAttributes(), field, et, fetchType, type, participationConstraints, converterWrapper);
        }

        et.addDeclaredQueryAttribute(field.getName(), a);
    }

    private AbstractAttribute<X, ?> createAttribute(PropertyInfo field, InferenceInfo inference,
                                                    PropertyAttributes propertyAttributes) {
        final AbstractAttribute<X, ?> a;
        if (field.getType().isAssignableFrom(Collection.class)) {
            final AbstractPluralAttribute.PluralAttributeBuilder builder =
                    CollectionAttributeImpl.builder(propertyAttributes).declaringType(et)
                            .propertyInfo(field)
                            .inferred(inference.inferred).includeExplicit(inference.includeExplicit);
            context.getConverterResolver().resolveConverter(field, propertyAttributes).ifPresent(builder::converter);
            a = (AbstractAttribute<X, ?>) builder.build();
        } else if (field.getType().isAssignableFrom(List.class)) {
            a = createListAttribute(field, inference, propertyAttributes);
        } else if (field.getType().isAssignableFrom(Set.class)) {
            final AbstractPluralAttribute.PluralAttributeBuilder builder =
                    SetAttributeImpl.builder(propertyAttributes).declaringType(et)
                            .propertyInfo(field)
                            .inferred(inference.inferred).includeExplicit(inference.includeExplicit);
            context.getConverterResolver().resolveConverter(field, propertyAttributes).ifPresent(builder::converter);
            a = (AbstractAttribute<X, ?>) builder.build();
        } else if (field.getType().isAssignableFrom(Map.class)) {
            throw new IllegalArgumentException("NOT YET SUPPORTED");
        } else {
            final SingularAttributeImpl.SingularAttributeBuilder builder =
                    SingularAttributeImpl.builder(propertyAttributes).declaringType(et)
                            .propertyInfo(field)
                            .inferred(inference.inferred).includeExplicit(inference.includeExplicit);
            context.getConverterResolver().resolveConverter(field, propertyAttributes).ifPresent(builder::converter);
            a = (AbstractAttribute<X, ?>) builder.build();
        }
        et.addDeclaredAttribute(field.getName(), a);
        return a;
    }

    private AbstractAttribute<X, ?> createListAttribute(PropertyInfo propertyInfo,
                                                        InferenceInfo inference,
                                                        PropertyAttributes propertyAttributes) {
        final Sequence os = propertyInfo.getAnnotation(Sequence.class);
        if (os == null) {
            throw new MetamodelInitializationException("Expected Sequence annotation.");
        }
        final ListAttributeImpl.ListAttributeBuilder builder =
                ListAttributeImpl.builder(propertyAttributes).declaringType(et)
                        .propertyInfo(propertyInfo)
                        .inferred(inference.inferred).includeExplicit(inference.includeExplicit)
                        .owlListClass(IRI.create(os.ClassOWLListIRI()))
                        .hasNextProperty(IRI.create(os.ObjectPropertyHasNextIRI()))
                        .hasContentsProperty(IRI.create(os.ObjectPropertyHasContentsIRI()))
                        .sequenceType(os.type());
        context.getConverterResolver().resolveConverter(propertyInfo, propertyAttributes).ifPresent(builder::converter);
        return builder.build();
    }

    private void registerTypeReference(Attribute<X, ?> attribute) {
        final Class<?> type = attribute.isCollection() ? ((PluralAttribute<X, ?, ?>) attribute).getBindableJavaType() :
                attribute.getJavaType();
        if (metamodelBuilder.hasManagedType(type)) {
            metamodelBuilder.registerTypeReference(type, et.getJavaType());
        }
    }

    private boolean tryProcessIdentifierField(Field field) {
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
