/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.Convert;
import cz.cvut.kbss.jopa.model.annotations.Enumerated;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.RDFCollection;
import cz.cvut.kbss.jopa.model.annotations.RDFContainer;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.Sparql;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.InferenceMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
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
        LOG.trace("processing field: {}.", field);
        if (EntityPropertiesUtils.isFieldTransient(field)) {
            LOG.trace("Skipping transient field {}.", field);
            return;
        }

        final Optional<Class<?>> fieldValueClassOpt = getFieldValueType(field);
        if (fieldValueClassOpt.isEmpty()) {
            // Defer generic field initialization
            metamodelBuilder.registerDeferredFieldInitialization(field, context);
            return;
        }
        processFieldWithValueType(field, fieldValueClassOpt.get(), field.getDeclaringClass());
    }

    private void processFieldWithValueType(Field field, Class<?> fieldValueCls, Class<?> sourceCls) {
        final InferenceMode inference = resolveInferenceMode(field);

        if (isTypesField(field)) {
            processTypesField(field, fieldValueCls, inference);
            return;
        }
        if (isPropertiesField(field)) {
            processPropertiesField(field, fieldValueCls, inference);
            return;
        }

        if (isQueryAttribute(field)) {
            createQueryAttribute(field, fieldValueCls, sourceCls);
            return;
        }

        PropertyInfo propertyInfo = PropertyInfo.from(field);

        final PropertyAttributes propertyAtt = PropertyAttributes.create(propertyInfo, mappingValidator, context);
        propertyAtt.resolve(propertyInfo, metamodelBuilder, fieldValueCls);

        if (propertyAtt.isKnownOwlProperty()) {
            final AbstractAttribute<X, ?> a = createAndDeclareAttribute(sourceCls, propertyInfo, inference, propertyAtt);
            registerTypeReference(a);
            return;
        }

        if (isIdentifierField(field)) {
            processIdentifierField(field);
            return;
        }

        AnnotatedAccessor fieldAccessor = findAnnotatedMethodBelongingToField(field);

        if (fieldAccessor != null) {
            createAndRegisterAttribute(field, inference, fieldValueCls, fieldAccessor, sourceCls);
            return;
        }

        throw new MetamodelInitializationException("Unable to process field " + field + ". It is not transient but has no mapping information.");
    }

    private AbstractAttribute<X, ?> createAndDeclareAttribute(Class<?> sourceCls, PropertyInfo propertyInfo,
                                                              InferenceMode inference, PropertyAttributes propertyAtt) {
        final AbstractAttribute<X, ?> a = createAttribute(propertyInfo, inference, propertyAtt);
        if (!Objects.equals(et.getJavaType(), sourceCls)) {
            et.addDeclaredGenericAttribute(a.getName(), sourceCls.asSubclass(et.getJavaType()), a);
        } else {
            et.addDeclaredAttribute(a.getName(), a);
        }
        return a;
    }


    /**
     * Do a bottom top search of hierarchy to find annotated accessor belonging to given field. This is used
     * when a property annotation (
     * {@link OWLDataProperty,OWLObjectProperty,OWLAnnotationProperty) is declared on method, not on field.
     */
    private AnnotatedAccessor findAnnotatedMethodBelongingToField(Field field) {
        LOG.debug("finding property definition to field {} ", field);

        Deque<IdentifiableType<?>> toVisit = new ArrayDeque<>();
        toVisit.add(et);

        boolean found = false;
        AnnotatedAccessor foundAccessor = null;

        while (!toVisit.isEmpty()) {
            IdentifiableType<?> top = toVisit.peek();
            Collection<AnnotatedAccessor> accessorsInParent = metamodelBuilder.getAnnotatedAccessorsForClass(top);

            for (AnnotatedAccessor annotatedAccessor : accessorsInParent) {

                if (!propertyBelongsToMethod(field, annotatedAccessor)) {
                    continue;
                }

                LOG.debug("Found annotated method belonging to field - {} - {}", annotatedAccessor.getMethod()
                                                                                                  .getName(), field.getName());
                if (!found) {  // first belonging method
                    found = true;
                    foundAccessor = annotatedAccessor;
                } else if (methodsAnnotationsEqual(annotatedAccessor.getMethod(), foundAccessor.getMethod())) {
                    LOG.debug("Methods are equal, skipping");
                } else { /// Two non-equal methods that could belong to the field - ambiguous
                    throw new MetamodelInitializationException("Ambiguous hierarchy - fields can inherit only from multiple methods if their property mapping annotations equal. However for field " + field + " two non-compatible methods were found - " + foundAccessor.getMethod() + " and " + annotatedAccessor.getMethod());
                }

            }
            /// remove top
            toVisit.pop();
            /// add all parents
            toVisit.addAll(top.getSupertypes());
        }

        return foundAccessor;
    }

    private void createAndRegisterAttribute(Field field, InferenceMode inference, Class<?> fieldValueCls,
                                            AnnotatedAccessor annotatedAccessor, Class<?> sourceCls) {
        final PropertyInfo info = PropertyInfo.from(annotatedAccessor.getMethod(), field);

        final PropertyAttributes propertyAtt = PropertyAttributes.create(info, mappingValidator, context);
        propertyAtt.resolve(info, metamodelBuilder, fieldValueCls);

        final AbstractAttribute<X, ?> a = createAndDeclareAttribute(sourceCls, info, inference, propertyAtt);
        registerTypeReference(a);
    }

    private static boolean methodsAnnotationsEqual(Method newMethod, Method foundMethod) {
        return Objects.equals(newMethod.getAnnotation(OWLObjectProperty.class), foundMethod.getAnnotation(OWLObjectProperty.class)) &&
                Objects.equals(newMethod.getAnnotation(OWLDataProperty.class), foundMethod.getAnnotation(OWLDataProperty.class)) &&
                Objects.equals(newMethod.getAnnotation(OWLAnnotationProperty.class), foundMethod.getAnnotation(OWLAnnotationProperty.class)) &&
                Objects.equals(newMethod.getAnnotation(Convert.class), foundMethod.getAnnotation(Convert.class)) &&
                Objects.equals(newMethod.getAnnotation(Sequence.class), foundMethod.getAnnotation(Sequence.class)) &&
                Objects.equals(newMethod.getAnnotation(Enumerated.class), foundMethod.getAnnotation(Enumerated.class)) &&
                Objects.equals(newMethod.getAnnotation(ParticipationConstraints.class), foundMethod.getAnnotation(ParticipationConstraints.class));

    }


    private static boolean propertyBelongsToMethod(Field property, AnnotatedAccessor accessor) {
        if (!property.getName().equals(accessor.getPropertyName())) {
            return false;
        }
        if (!property.getType().isAssignableFrom(accessor.getPropertyType())) {
            throw new MetamodelInitializationException("Non-compatible types between method " + accessor + " and field " + property.getName() + ". Method is accessor to type " + accessor.getPropertyType() + ", field type is " + property.getType());
        }

        return true;
    }

    private static Optional<Class<?>> getFieldValueType(Field field) {
        if (Collection.class.isAssignableFrom(field.getType())) {
            return getSetOrListErasureType((ParameterizedType) field.getGenericType());
        } else if (field.getType().isArray()) {
            throw new MetamodelInitializationException("Array persistent attributes are not supported.");
        } else {
            // If the type is generic, defer its resolution until the metamodel is built
            return field.getGenericType() instanceof TypeVariable<?> ? Optional.empty() : Optional.of(field.getType());
        }
    }

    private static Optional<Class<?>> getSetOrListErasureType(final ParameterizedType cls) {
        final Type[] t = cls.getActualTypeArguments();

        if (t.length != 1) {
            throw new OWLPersistenceException("Only collections with a single generic parameter are supported.");
        }
        Type type = t[0];
        if (type instanceof Class<?>) {
            return Optional.of((Class<?>) type);
        } else if (type instanceof ParameterizedType) {
            final Type rawType = ((ParameterizedType) type).getRawType();
            return Optional.of((Class<?>) rawType);
        } else if (type instanceof TypeVariable) {
            return Optional.empty();
        }
        throw new OWLPersistenceException("Unsupported collection element type " + type);
    }

    private InferenceMode resolveInferenceMode(Field field) {
        final Inferred inferred = field.getAnnotation(Inferred.class);

        final InferenceMode result;
        if (inferred != null) {
            if (inferred.includeExplicit()) {
                result = InferenceMode.EXPLICIT_AND_INFERRED;
            } else {
                result = InferenceMode.INFERRED;
            }
        } else {
            result = InferenceMode.EXPLICIT;
        }
        if (result != InferenceMode.EXPLICIT) {
            metamodelBuilder.addInferredClass(cls);
        }
        return result;
    }

    private static boolean isTypesField(Field field) {
        return field.getAnnotation(Types.class) != null;
    }

    private void processTypesField(Field field, Class<?> fieldValueCls, InferenceMode inference) {
        mappingValidator.validateTypesField(field);
        // Always use eager for Types, they are fetched eagerly anyway to ensure entity is of correct type
        final FetchType fetchType = FetchType.EAGER;
        et.addDirectTypes(new TypesSpecificationImpl<>(et, fetchType, field, fieldValueCls, inference));
    }

    private static boolean isPropertiesField(Field field) {
        return field.getAnnotation(Properties.class) != null;
    }

    private void processPropertiesField(Field field, Class<?> fieldValueCls, InferenceMode inference) {
        Properties properties = field.getAnnotation(Properties.class);
        mappingValidator.validatePropertiesField(field);
        final PropertiesParametersResolver paramsResolver = new PropertiesParametersResolver(field);
        final FetchType fetchType = inference != InferenceMode.EXPLICIT ? FetchType.EAGER : properties.fetchType();
        et.addOtherProperties(
                PropertiesSpecificationImpl.declaringType(et).fetchType(fetchType).javaField(field)
                                           .javaType(fieldValueCls).inferenceMode(inference)
                                           .propertyIdType(paramsResolver.getPropertyIdentifierType())
                                           .propertyValueType(paramsResolver.getPropertyValueType()).build());
    }

    private static boolean isQueryAttribute(Field field) {
        return field.getAnnotation(Sparql.class) != null;
    }

    private void createQueryAttribute(Field field, Class<?> fieldValueCls, Class<?> sourceCls) {
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

        if (!Objects.equals(et.getJavaType(), sourceCls)) {
            et.addDeclaredGenericQueryAttribute(field.getName(), sourceCls.asSubclass(et.getJavaType()), a);
        } else {
            et.addDeclaredQueryAttribute(field.getName(), a);
        }
    }

    private AbstractAttribute<X, ?> createAttribute(PropertyInfo property, InferenceMode inference, PropertyAttributes propertyAttributes) {
        final AbstractAttribute<X, ?> a;
        if (property.getAnnotation(RDFContainer.class) != null) {
            a = createRdfContainerAttribute(property, inference, propertyAttributes);
        } else if (property.getType().isAssignableFrom(Object.class)) {
            final SingularAttributeImpl.SingularAttributeBuilder builder = setCommonBuildParameters(SingularAttributeImpl.builder(propertyAttributes),
                    property, inference);
            context.getConverterResolver().resolveConverter(property, propertyAttributes).ifPresent(builder::converter);
            a = builder.build();
        } else if (property.getType().isAssignableFrom(Collection.class)) {
            final AbstractPluralAttribute.PluralAttributeBuilder builder = setCommonBuildParameters(CollectionAttributeImpl.builder(propertyAttributes),
                    property, inference);
            context.getConverterResolver().resolveConverter(property, propertyAttributes).ifPresent(builder::converter);
            a = builder.build();
        } else if (property.getType().isAssignableFrom(List.class)) {
            a = createListAttribute(property, inference, propertyAttributes);
        } else if (property.getType().isAssignableFrom(Set.class)) {
            final AbstractPluralAttribute.PluralAttributeBuilder builder = setCommonBuildParameters(SetAttributeImpl.builder(propertyAttributes),
                    property, inference);
            context.getConverterResolver().resolveConverter(property, propertyAttributes).ifPresent(builder::converter);
            a = builder.build();
        } else if (property.getType().isAssignableFrom(Map.class)) {
            throw new IllegalArgumentException("NOT YET SUPPORTED");
        } else {
            final SingularAttributeImpl.SingularAttributeBuilder builder = setCommonBuildParameters(SingularAttributeImpl.builder(propertyAttributes),
                    property, inference);
            context.getConverterResolver().resolveConverter(property, propertyAttributes).ifPresent(builder::converter);
            a = builder.build();
        }
        return a;
    }

    private <B extends AbstractAttribute.AbstractAttributeBuilder<X, ?>> B setCommonBuildParameters(B builder,
                                                                                                    PropertyInfo propertyInfo,
                                                                                                    InferenceMode inference) {
        builder.declaringType(et)
               .propertyInfo(propertyInfo)
                .inferenceMode(inference);
        return builder;
    }

    private AbstractAttribute<X, ?> createRdfContainerAttribute(PropertyInfo property, InferenceMode inference,
                                                                PropertyAttributes propertyAttributes) {
        if (inference != InferenceMode.EXPLICIT) {
            throw new InvalidFieldMappingException("RDF container attributes cannot be inferred. Attribute: " + property.getName());
        }
        final RDFContainer rdfContainer = property.getAnnotation(RDFContainer.class);
        assert rdfContainer != null;
        final RdfContainerAttributeImpl.RDFContainerAttributeBuilder builder = setCommonBuildParameters(RdfContainerAttributeImpl.builder(propertyAttributes),
                property, inference)
                .containerType(rdfContainer.type())
                .collectionType(property.getType());
        context.getConverterResolver().resolveConverter(property, propertyAttributes).ifPresent(builder::converter);
        return builder.build();
    }

    private AbstractAttribute<X, ?> createListAttribute(PropertyInfo property, InferenceMode
            inference, PropertyAttributes propertyAttributes) {
        if (property.getAnnotation(RDFCollection.class) != null) {
            return createRdfCollectionAttribute(property, inference, propertyAttributes);
        }
        final Sequence os = property.getAnnotation(Sequence.class);
        if (os == null) {
            throw new MetamodelInitializationException("Expected Sequence annotation.");
        }
        final ListAttributeImpl.ListAttributeBuilder builder = setCommonBuildParameters(ListAttributeImpl.builder(propertyAttributes),
                property, inference)
                .owlListClass(IRI.create(resolvePrefix(os.listClassIRI())))
                .hasNextProperty(IRI.create(resolvePrefix(os.hasNextPropertyIRI())))
                .hasContentsProperty(IRI.create(resolvePrefix(os.hasContentsPropertyIRI())))
                .sequenceType(os.type());
        context.getConverterResolver().resolveConverter(property, propertyAttributes).ifPresent(builder::converter);
        return builder.build();
    }

    private AbstractAttribute<X, ?> createRdfCollectionAttribute(PropertyInfo property, InferenceMode inference,
                                                                 PropertyAttributes propertyAttributes) {
        final RDFCollectionAttribute.RDFCollectionAttributeBuilder builder = setCommonBuildParameters(RDFCollectionAttribute.builder(propertyAttributes),
                property, inference);
        context.getConverterResolver().resolveConverter(property, propertyAttributes).ifPresent(builder::converter);
        return builder.build();
    }

    private String resolvePrefix(String value) {
        return context.resolveNamespace(value);
    }

    private void registerTypeReference(Attribute<X, ?> attribute) {
        final Class<?> type = attribute.isCollection() ? ((PluralAttribute<X, ?, ?>) attribute).getBindableJavaType() : attribute.getJavaType();
        if (metamodelBuilder.hasManagedType(type)) {
            metamodelBuilder.registerTypeReference(type, et.getJavaType());
        }
    }

    private void processIdentifierField(Field field) {
        final Id id = field.getAnnotation(Id.class);
        assert id != null;

        mappingValidator.validateIdentifierType(field.getType());
        et.setIdentifier(new IRIIdentifierImpl<>(et, field, id.generated()));
    }

    private static boolean isIdentifierField(Field field) {
        return field.getAnnotation(Id.class) != null;
    }

    void processDeferredField(Field field) {
        if (field.getGenericType() instanceof TypeVariable<?> || Collection.class.isAssignableFrom(field.getType())) {
            for (AbstractIdentifiableType<?> st : et.getSubtypes()) {
                recursivelyProcessDeferredField(field, st);
            }
        }
    }

    private void recursivelyProcessDeferredField(Field field, AbstractIdentifiableType<?> type) {
        if (type.isAbstract()) {
            type.getSubtypes().forEach(st -> recursivelyProcessDeferredField(field, st));
        } else {
            assert type.getJavaType().getGenericSuperclass() instanceof ParameterizedType;
            final String typeVar = resolveTypeVariable(field);

            final ParameterizedType t = (ParameterizedType) type.getJavaType().getGenericSuperclass();
            final TypeVariable<?>[] typeVars = ((Class<?>) t.getRawType()).getTypeParameters();
            assert typeVars.length == t.getActualTypeArguments().length;
            // Match actual type parameter with type variable name in field declaration
            for (int i = 0; i < t.getActualTypeArguments().length; i++) {
                if (typeVar.equals(typeVars[i].getName())) {
                    processFieldWithValueType(field, (Class<?>) t.getActualTypeArguments()[i], type.getJavaType());
                    break;
                }
            }
        }
    }

    private static String resolveTypeVariable(Field field) {
        final String typeVar;
        if (field.getGenericType() instanceof ParameterizedType pt) {
            // Collections - e.g. Set<T>
            typeVar = pt.getActualTypeArguments()[0].getTypeName();
        } else if (field.getGenericType() instanceof TypeVariable<?> tv) {
            // Singular attributes - e.g. T
            typeVar = tv.getTypeName();
        } else {
            throw new MetamodelInitializationException("Unsupported generic type " + field.getGenericType() + " of field " + field);
        }
        return typeVar;
    }
}
