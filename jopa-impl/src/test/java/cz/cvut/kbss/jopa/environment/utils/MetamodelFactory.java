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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.OWLClassG;
import cz.cvut.kbss.jopa.environment.OWLClassH;
import cz.cvut.kbss.jopa.environment.OWLClassI;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassK;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.OWLClassO;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.OWLClassT;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.OWLClassW;
import cz.cvut.kbss.jopa.environment.OWLClassWithQueryAttr;
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.Person;
import cz.cvut.kbss.jopa.environment.Phone;
import cz.cvut.kbss.jopa.environment.QMappedSuperclass;
import cz.cvut.kbss.jopa.environment.ZoneOffsetConverter;
import cz.cvut.kbss.jopa.environment.listener.AnotherListener;
import cz.cvut.kbss.jopa.environment.listener.ConcreteListener;
import cz.cvut.kbss.jopa.environment.listener.ParentListener;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.Sparql;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.AbstractIdentifiableType;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.AbstractQueryAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.BasicTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.Bindable;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.EntityLifecycleListenerManager;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.MappedSuperclassTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.RDFCollectionAttribute;
import cz.cvut.kbss.jopa.model.metamodel.RdfContainerAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.gen.ManageableClassGenerator;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistenceContextAwareClassGenerator;
import cz.cvut.kbss.jopa.oom.converter.CharacterConverter;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.CustomConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.DefaultConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.ObjectOneOfEnumConverter;
import cz.cvut.kbss.jopa.oom.converter.OrdinalEnumConverter;
import cz.cvut.kbss.jopa.oom.converter.StringEnumConverter;
import cz.cvut.kbss.jopa.oom.converter.ToIntegerConverter;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.LocalDateTimeConverter;
import cz.cvut.kbss.jopa.utils.Configuration;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent.POST_LOAD;
import static cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent.POST_PERSIST;
import static cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent.POST_REMOVE;
import static cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent.POST_UPDATE;
import static cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent.PRE_PERSIST;
import static cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent.PRE_REMOVE;
import static cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent.PRE_UPDATE;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

/**
 * Initializes the specified mock objects to return reasonable values.
 */
@SuppressWarnings({"unchecked", "rawtypes"})
public class MetamodelFactory {

    private static PersistenceContextAwareClassGenerator instantiableTypeGenerator;

    static {
        reset();
    }

    private MetamodelFactory() {
    }

    public static void reset() {
        instantiableTypeGenerator = new ManageableClassGenerator(new Configuration());
    }

    public static void setInstantiableTypeGenerator(PersistenceContextAwareClassGenerator generator) {
        instantiableTypeGenerator = generator;
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassAMocks(IdentifiableEntityType<OWLClassA> etMock, SingularAttributeImpl strAttMock,
                                          TypesSpecification typesMock, Identifier idMock) throws Exception {
        final EntityLifecycleListenerManager listenerManager = new EntityLifecycleListenerManager();
        addLifecycleCallback(listenerManager, POST_LOAD, OWLClassA.getPostLoadCallback());
        when(etMock.getLifecycleListenerManager()).thenReturn(listenerManager);
        when(etMock.getDeclaredAttribute(anyString())).thenAnswer(args -> {
            final String name = args.getArgument(0);
            if (Objects.equals(name, strAttMock.getName())) {
                return strAttMock;
            }
            throw new IllegalArgumentException("Unknown attribute " + name);
        });
        initEntityType(etMock, OWLClassA.class, listenerManager);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(strAttMock, typesMock, idMock));

        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassA.getStrAttField(), Attribute.PersistentAttributeType.DATA)
                .language(Generators.LANG));
        initTypesAttribute(etMock, typesMock, new AttributeInfo(OWLClassA.getTypesField(), null).elementType(String.class));

        initIdentifier(etMock, idMock, OWLClassA.class.getDeclaredField("uri"), false);
    }

    private static <X> void initEntityType(IdentifiableEntityType<X> et, Class<X> cls, EntityLifecycleListenerManager listenerManager) {
        when(et.getJavaType()).thenReturn(cls);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(cls));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(et.getIRI()).thenReturn(IRI.create(cls.getAnnotation(OWLClass.class).iri()));
        when(et.getName()).thenReturn(cls.getSimpleName());
        when(et.getLifecycleListenerManager()).thenReturn(listenerManager);
    }

    private static <X> void initIdentifier(IdentifiableEntityType<X> et, Identifier id, Field idField,
                                           boolean generated) {
        when(et.getIdentifier()).thenReturn(id);
        when(id.getJavaField()).thenReturn(idField);
        when(id.getDeclaringType()).thenReturn(et);
        when(id.getName()).thenReturn(idField.getName());
        when(id.isGenerated()).thenReturn(generated);
        when(id.includeExplicit()).thenReturn(true);
        when(et.getFieldSpecification(idField.getName())).thenReturn(id);
    }

    private static <X> void initAttribute(IdentifiableEntityType<X> etMock, AbstractAttribute attMock,
                                          AttributeInfo attInfo) {
        when(attMock.isMappedAttribute()).thenReturn(true);
        when(etMock.getFieldSpecification(attInfo.field.getName())).thenReturn(attMock);
        when(etMock.getAttribute(attInfo.field.getName())).thenReturn(attMock);
        when(attMock.getName()).thenReturn(attInfo.field.getName());
        when(attMock.getDeclaringType()).thenReturn(etMock);
        when(attMock.getJavaField()).thenReturn(attInfo.field);
        when(attMock.getJavaType()).thenReturn(attInfo.field.getType());
        when(attMock.getConstraints()).thenReturn(attInfo.constraints);
        when(attMock.isNonEmpty()).thenReturn(attInfo.nonEmpty);
        when(attMock.getPersistentAttributeType()).thenReturn(attInfo.type);
        when(attMock.getConverter()).thenReturn(attInfo.converter);
        when(attMock.includeExplicit()).thenReturn(true);
        when(attMock.isCollection()).thenReturn(Collection.class.isAssignableFrom(attInfo.field.getType()));
        when(attMock.getJavaMember()).thenReturn(attInfo.field);
        if (attMock instanceof SingularAttributeImpl) {
            initSingularAttribute((SingularAttributeImpl) attMock, attInfo);
        } else {
            initPluralAttribute((AbstractPluralAttribute) attMock, attInfo);
        }
        switch (attInfo.type) {
            case DATA:
                final OWLDataProperty dp = attInfo.field.getAnnotation(OWLDataProperty.class);
                when(attMock.getIRI()).thenReturn(IRI.create(dp.iri()));
                when(attMock.getFetchType()).thenReturn(dp.fetch());
                when(attMock.isSimpleLiteral()).thenReturn(dp.simpleLiteral());
                when(attMock.isLexicalForm()).thenReturn(dp.lexicalForm());
                when(attMock.isAssociation()).thenReturn(false);
                when(attMock.getDatatype()).thenReturn(dp.datatype());
                when(attMock.isInferred()).thenReturn(attInfo.field.getAnnotation(Inferred.class) != null);
                when(attMock.getCascadeTypes()).thenReturn(new CascadeType[0]);
                when(attMock.getLanguage()).thenReturn(attInfo.language);
                when(attMock.hasLanguage()).thenReturn(attInfo.language != null);
                break;
            case OBJECT:
                final OWLObjectProperty op = attInfo.field.getAnnotation(OWLObjectProperty.class);
                when(attMock.getIRI()).thenReturn(IRI.create(op.iri()));
                when(attMock.getFetchType()).thenReturn(op.fetch());
                when(attMock.isAssociation()).thenReturn(true);
                when(attMock.isInferred()).thenReturn(attInfo.field.getAnnotation(Inferred.class) != null);
                when(attMock.getCascadeTypes()).thenReturn(op.cascade());
                break;
            case ANNOTATION:
                final OWLAnnotationProperty ap = attInfo.field.getAnnotation(OWLAnnotationProperty.class);
                when(attMock.getIRI()).thenReturn(IRI.create(ap.iri()));
                when(attMock.getFetchType()).thenReturn(ap.fetch());
                when(attMock.isSimpleLiteral()).thenReturn(ap.simpleLiteral());
                when(attMock.isLexicalForm()).thenReturn(ap.lexicalForm());
                when(attMock.isAssociation()).thenReturn(false);
                when(attMock.getDatatype()).thenReturn(ap.datatype());
                when(attMock.isInferred()).thenReturn(attInfo.field.getAnnotation(Inferred.class) != null);
                when(attMock.getCascadeTypes()).thenReturn(new CascadeType[0]);
                when(attMock.getLanguage()).thenReturn(attInfo.language);
                when(attMock.hasLanguage()).thenReturn(attInfo.language != null);
                break;
        }
    }

    private static void initSingularAttribute(SingularAttributeImpl attMock, AttributeInfo attInfo) {
        when(attMock.getBindableJavaType()).thenReturn(attInfo.field.getType());
        when(attMock.getType()).thenReturn(Objects.requireNonNullElseGet(attInfo.valueType, () -> BasicTypeImpl.get(attInfo.field.getType())));
    }

    private static void initPluralAttribute(AbstractPluralAttribute attMock, AttributeInfo attInfo) {
        when(attMock.getBindableJavaType()).thenReturn(attInfo.elementType);
        when(attMock.getBindableType()).thenReturn(Bindable.BindableType.PLURAL_ATTRIBUTE);
        when(attMock.getCollectionType()).thenReturn(attInfo.collectionType);
        when(attMock.getElementType()).thenReturn(attInfo.valueType);
    }

    private static void addLifecycleCallback(EntityLifecycleListenerManager manager, LifecycleEvent evt,
                                             Method callback) throws Exception {
        final Method addCallback = EntityLifecycleListenerManager.class
                .getDeclaredMethod("addLifecycleCallback", LifecycleEvent.class, Method.class);
        if (!addCallback.canAccess(manager)) {
            addCallback.setAccessible(true);
        }
        addCallback.invoke(manager, evt, callback);
    }

    private static <X> void initTypesAttribute(IdentifiableEntityType<X> etMock, TypesSpecification typesMock,
                                               AttributeInfo attInfo) {
        when(typesMock.getJavaField()).thenReturn(attInfo.field);
        when(typesMock.getName()).thenReturn(attInfo.field.getName());
        when(typesMock.getDeclaringType()).thenReturn(etMock);
        when(typesMock.getJavaType()).thenReturn(Set.class);
        when(typesMock.getElementType()).thenReturn(attInfo.elementType);
        when(typesMock.isCollection()).thenReturn(true);
        when(typesMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(typesMock.includeExplicit()).thenReturn(true);
        when(typesMock.getJavaField()).thenReturn(attInfo.field);
        when(etMock.getFieldSpecification(attInfo.field.getName())).thenReturn(typesMock);
        when(etMock.getTypes()).thenReturn(typesMock);
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassBMocks(IdentifiableEntityType<OWLClassB> etMock, SingularAttributeImpl strAttMock,
                                          PropertiesSpecification propsMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassB.class, EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(strAttMock, propsMock, idMock));

        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassB.getStrAttField(), Attribute.PersistentAttributeType.DATA)
                .language(Generators.LANG));
        initProperties(etMock, propsMock, new AttributeInfo(OWLClassB.getPropertiesField(), null), String.class, String.class);

        initIdentifier(etMock, idMock, OWLClassB.class.getDeclaredField("uri"), false);
        when(etMock.getDeclaredAttribute(anyString())).thenAnswer(args -> {
            final String name = args.getArgument(0);
            if (Objects.equals(name, strAttMock.getName())) {
                return strAttMock;
            }
            throw new IllegalArgumentException("Unknown attribute " + name);
        });
    }

    private static <X> void initProperties(IdentifiableEntityType<X> etMock, PropertiesSpecification propsMock,
                                           AttributeInfo attInfo, Class<?> propertyType, Class<?> valueType) {
        when(propsMock.getJavaField()).thenReturn(attInfo.field);
        when(propsMock.getJavaType()).thenReturn(attInfo.field.getType());
        when(propsMock.getName()).thenReturn(attInfo.field.getName());
        when(propsMock.getDeclaringType()).thenReturn(etMock);
        when(propsMock.getPropertyIdentifierType()).thenReturn(propertyType);
        when(propsMock.getPropertyValueType()).thenReturn(valueType);
        when(propsMock.getJavaField()).thenReturn(attInfo.field);
        when(etMock.getFieldSpecification(propsMock.getName())).thenReturn(propsMock);
        when(etMock.getProperties()).thenReturn(propsMock);
    }

    public static void initOWLClassCMocks(IdentifiableEntityType<OWLClassC> etMock,
                                          ListAttributeImpl simpleListMock, ListAttributeImpl refListMock,
                                          RDFCollectionAttribute rdfCollectionMock,
                                          RdfContainerAttributeImpl rdfSeqMock,
                                          IdentifiableEntityType<OWLClassA> etAMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassC.class, EntityLifecycleListenerManager.empty());

        when(etMock.getAttributes()).thenReturn(Set.of(simpleListMock, refListMock, rdfSeqMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(simpleListMock, refListMock, rdfSeqMock, idMock));
        initListAttribute(etMock, simpleListMock, new AttributeInfo(OWLClassC.getSimpleListField(), Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.LIST)
                                                                                                                                             .elementType(OWLClassA.class)
                                                                                                                                             .valueType(etAMock));
        initListAttribute(etMock, refListMock, new AttributeInfo(OWLClassC.getRefListField(), Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.LIST)
                                                                                                                                       .elementType(OWLClassA.class)
                                                                                                                                       .valueType(etAMock));
        when(etMock.getAttribute(OWLClassC.getRdfCollectionField().getName())).thenReturn(rdfCollectionMock);
        when(rdfCollectionMock.getJavaField()).thenReturn(OWLClassC.getRdfCollectionField());
        when(rdfCollectionMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(rdfCollectionMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(rdfCollectionMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(rdfCollectionMock.getName()).thenReturn(OWLClassC.getRdfCollectionField().getName());
        when(etMock.getFieldSpecification(rdfCollectionMock.getName())).thenReturn(rdfCollectionMock);
        when(rdfCollectionMock.getIRI()).thenReturn(IRI.create(OWLClassC.getRdfCollectionField()
                                                                                       .getAnnotation(OWLObjectProperty.class).iri()));
        when(rdfCollectionMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(rdfCollectionMock.getElementType()).thenReturn(etAMock);
        when(rdfCollectionMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(rdfCollectionMock.isCollection()).thenReturn(true);
        when(rdfCollectionMock.isAssociation()).thenReturn(true);
        when(rdfCollectionMock.isRDFCollection()).thenReturn(true);
        when(rdfCollectionMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(rdfCollectionMock.getDeclaringType()).thenReturn(etMock);
        when(rdfCollectionMock.getJavaType()).thenReturn(List.class);
        when(rdfCollectionMock.isMappedAttribute()).thenReturn(true);

        when(etMock.getAttribute(OWLClassC.getRdfSeqField().getName())).thenReturn(rdfSeqMock);
        when(rdfSeqMock.getJavaField()).thenReturn(OWLClassC.getRdfSeqField());
        when(rdfSeqMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(rdfSeqMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(rdfSeqMock.getContainerType()).thenReturn(RDFContainerType.SEQ);
        when(rdfSeqMock.getName()).thenReturn(OWLClassC.getRdfSeqField().getName());
        when(etMock.getFieldSpecification(rdfSeqMock.getName())).thenReturn(rdfSeqMock);
        when(rdfSeqMock.getIRI()).thenReturn(IRI.create(OWLClassC.getRdfSeqField()
                                                                 .getAnnotation(OWLObjectProperty.class).iri()));
        when(rdfSeqMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(rdfSeqMock.getElementType()).thenReturn(etAMock);
        when(rdfSeqMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(rdfSeqMock.isCollection()).thenReturn(true);
        when(rdfSeqMock.isAssociation()).thenReturn(true);
        when(rdfSeqMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(rdfSeqMock.getDeclaringType()).thenReturn(etMock);
        when(rdfSeqMock.getJavaType()).thenReturn(List.class);
        when(rdfSeqMock.getJavaMember()).thenReturn(OWLClassC.getRdfSeqField());
        when(rdfSeqMock.isRdfContainer()).thenReturn(true);
        when(rdfSeqMock.isMappedAttribute()).thenReturn(true);
        when(rdfSeqMock.getCascadeTypes())
                .thenReturn(OWLClassC.getRdfSeqField().getAnnotation(OWLObjectProperty.class).cascade());


        initIdentifier(etMock, idMock, OWLClassC.class.getDeclaredField("uri"), false);
        when(etMock.getDeclaredAttribute(anyString())).thenAnswer(args -> {
            final String name = args.getArgument(0);
            if (Objects.equals(name, refListMock.getName())) {
                return refListMock;
            } else if (Objects.equals(name, simpleListMock.getName())) {
                return simpleListMock;
            } else if (Objects.equals(name, rdfSeqMock.getName())) {
                return rdfSeqMock;
            }
            throw new IllegalArgumentException("Unknown attribute " + name);
        });
    }

    private static <X> void initListAttribute(IdentifiableEntityType<X> etMock, ListAttributeImpl attMock,
                                              AttributeInfo attInfo) {
        initAttribute(etMock, attMock, attInfo);
        final Sequence seq = attInfo.field.getAnnotation(Sequence.class);
        when(attMock.getHasNextPropertyIRI()).thenReturn(IRI.create(seq.hasNextPropertyIRI()));
        when(attMock.getHasContentsPropertyIRI()).thenReturn(IRI.create(seq.hasContentsPropertyIRI()));
        when(attMock.getListClassIRI()).thenReturn(IRI.create(seq.listClassIRI()));
        when(attMock.getSequenceType()).thenReturn(seq.type());
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassDMocks(IdentifiableEntityType<OWLClassD> etMock, SingularAttributeImpl clsAMock,
                                          IdentifiableEntityType<OWLClassA> etA, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassD.class, EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(clsAMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(clsAMock, idMock));
        initAttribute(etMock, clsAMock, new AttributeInfo(OWLClassD.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT).valueType(etA));
        initIdentifier(etMock, idMock, OWLClassD.getUriField(), false);
    }

    public static void initOWLClassEMocks(IdentifiableEntityType<OWLClassE> etMock, SingularAttributeImpl strAttMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassE.class, EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(strAttMock, idMock));
        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassE.getStrAttField(), Attribute.PersistentAttributeType.DATA)
                .language(Generators.LANG));
        initIdentifier(etMock, idMock, OWLClassE.class.getDeclaredField("uri"), true);
    }

    public static void initOWLClassFMocks(IdentifiableEntityType<OWLClassF> etMock, AbstractPluralAttribute setAMock,
                                          SingularAttributeImpl strAttMock, IdentifiableEntityType<OWLClassA> etAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassF.class, EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Set.of(setAMock, strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(setAMock, strAttMock, idMock));
        initAttribute(etMock, setAMock, new AttributeInfo(OWLClassF.getSimpleSetField(),
                Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.SET)
                                                         .elementType(OWLClassA.class).valueType(etAMock));
        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassF.getStrAttField(), Attribute.PersistentAttributeType.DATA)
                .language(Generators.LANG));

        initIdentifier(etMock, idMock, OWLClassF.class.getDeclaredField("uri"), false);
    }

    public static void initOWLClassGMocks(IdentifiableEntityType<OWLClassG> etMock, SingularAttributeImpl clsHMock,
                                          IdentifiableEntityType<OWLClassH> etHMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassG.class, EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(clsHMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(clsHMock, idMock));
        initAttribute(etMock, clsHMock, new AttributeInfo(OWLClassG.getOwlClassHField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etHMock));
        initIdentifier(etMock, idMock, OWLClassG.class.getDeclaredField("uri"), false);
    }

    public static void initOWLClassHMocks(IdentifiableEntityType<OWLClassH> etMock, SingularAttributeImpl clsAMock,
                                          SingularAttributeImpl clsGMock, IdentifiableEntityType<OWLClassA> etAMock,
                                          IdentifiableEntityType<OWLClassG> etGMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassH.class,  EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Set.of(clsAMock, clsGMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(clsAMock, clsGMock, idMock));

        initAttribute(etMock, clsAMock, new AttributeInfo(OWLClassH.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etAMock));
        initAttribute(etMock, clsGMock, new AttributeInfo(OWLClassH.getOwlClassGField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etGMock));
        initIdentifier(etMock, idMock, OWLClassH.class.getDeclaredField("uri"), false);
    }

    public static void initOWLClassIMocks(IdentifiableEntityType<OWLClassI> etMock, SingularAttributeImpl aAttMock,
                                          IdentifiableEntityType<OWLClassA> etAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassI.class,  EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(aAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(aAttMock, idMock));

        initAttribute(etMock, aAttMock, new AttributeInfo(OWLClassI.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etAMock));
        initIdentifier(etMock, idMock, OWLClassI.class.getDeclaredField("uri"), false);
    }

    public static void initOWLClassJMocks(IdentifiableEntityType<OWLClassJ> etMock, AbstractPluralAttribute setAMock,
                                          IdentifiableEntityType<OWLClassA> etAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassJ.class,EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(setAMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(setAMock, idMock));
        initAttribute(etMock, setAMock, new AttributeInfo(OWLClassJ.getOwlClassAField(),
                Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.SET)
                                                         .elementType(OWLClassA.class).valueType(etAMock)
                .constraints(new ParticipationConstraint() {
                    @Override
                    public Class<? extends Annotation> annotationType() {
                        return ParticipationConstraint.class;
                    }

                    @Override
                    public String owlObjectIRI() {
                        return "";
                    }

                    @Override
                    public int min() {
                        return 1;
                    }

                    @Override
                    public int max() {
                        return -1;
                    }
                }));
        initIdentifier(etMock, idMock, OWLClassJ.class.getDeclaredField("uri"), false);
    }

    public static void initOWLClassKMocks(IdentifiableEntityType<OWLClassK> etMock, SingularAttributeImpl clsEMock,
                                          IdentifiableEntityType<OWLClassE> etEMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, OWLClassK.class, EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(clsEMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(clsEMock, idMock));

        initAttribute(etMock, clsEMock, new AttributeInfo(OWLClassK.getOwlClassEField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etEMock));
        initIdentifier(etMock, idMock, OWLClassK.class.getDeclaredField("uri"), false);
    }

    public static void initOWLClassLMocks(IdentifiableEntityType<OWLClassL> etMock, ListAttributeImpl refListMock,
                                          ListAttributeImpl simpleListMock, AbstractPluralAttribute setMock,
                                          SingularAttributeImpl singleAMock, IdentifiableEntityType<OWLClassA> etAMock,
                                          Identifier idMock) throws NoSuchFieldException {
        initEntityType(etMock, OWLClassL.class, EntityLifecycleListenerManager.empty());
        initIdentifier(etMock, idMock, OWLClassL.class.getDeclaredField("uri"), false);
        when(etMock.getDeclaredAttributes()).thenReturn(Set.of(refListMock, simpleListMock, setMock, singleAMock));
        when(etMock.getAttributes()).thenReturn(Set.of(refListMock, simpleListMock, setMock, singleAMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(refListMock, simpleListMock, setMock, singleAMock, idMock));

        initListAttribute(etMock, refListMock, new AttributeInfo(OWLClassL.getReferencedListField(), Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.LIST)
                                                                                                                                              .elementType(OWLClassA.class)
                                                                                                                                              .valueType(etAMock)
                                                                                                                                              .constraints(OWLClassL.getReferencedListField()
                                                                                                                                                                    .getAnnotation(ParticipationConstraints.class)
                                                                                                                                                                    .value()));
        initListAttribute(etMock, simpleListMock, new AttributeInfo(OWLClassL.getSimpleListField(), Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.LIST)
                                                                                                                                             .elementType(OWLClassA.class)
                                                                                                                                             .valueType(etAMock)
                                                                                                                                             .constraints(OWLClassL.getSimpleListField()
                                                                                                                                                                   .getAnnotation(ParticipationConstraints.class)
                                                                                                                                                                   .value()));
        initAttribute(etMock, setMock, new AttributeInfo(OWLClassL.getSetField(),
                Attribute.PersistentAttributeType.OBJECT).valueType(etAMock).elementType(OWLClassA.class)
                                                         .collectionType(CollectionType.SET)
                                                         .constraints(OWLClassL.getSetField()
                                                                               .getAnnotation(ParticipationConstraints.class)
                                                                               .value()));
        initAttribute(etMock, singleAMock, new AttributeInfo(OWLClassL.getSingleAField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etAMock).nonEmpty());
    }

    public static void initOWLClassMMock(IdentifiableEntityType<OWLClassM> etMock, SingularAttributeImpl booleanAtt,
                                         SingularAttributeImpl intAtt, SingularAttributeImpl longAtt,
                                         SingularAttributeImpl doubleAtt, SingularAttributeImpl dateAtt,
                                         SingularAttributeImpl characterAtt,
                                         SingularAttributeImpl enumAtt, SingularAttributeImpl ordinalEnumAtt,
                                         AbstractPluralAttribute intSetAtt, SingularAttributeImpl lexicalFormAtt,
                                         SingularAttributeImpl simpleLiteralAtt,
                                         SingularAttributeImpl explicitDatatypeAtt,
                                         SingularAttributeImpl mWithConverterAtt,
                                         SingularAttributeImpl mObjectOneOfEnumAttribute,
                                         Identifier idMock)
            throws Exception {
        initEntityType(etMock, OWLClassM.class, EntityLifecycleListenerManager.empty());
        initIdentifier(etMock, idMock, OWLClassM.getUriField(), false);
        when(etMock.getAttributes()).thenReturn(Set.of(booleanAtt, intAtt, longAtt, doubleAtt,
                dateAtt, characterAtt, enumAtt, ordinalEnumAtt, intSetAtt, lexicalFormAtt,
                simpleLiteralAtt, explicitDatatypeAtt, mObjectOneOfEnumAttribute));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(booleanAtt, intAtt, longAtt, doubleAtt, dateAtt,
                characterAtt, enumAtt, ordinalEnumAtt, intSetAtt, lexicalFormAtt, simpleLiteralAtt,
                explicitDatatypeAtt, mObjectOneOfEnumAttribute, idMock));

        initAttribute(etMock, booleanAtt, new AttributeInfo(OWLClassM.getBooleanAttributeField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(etMock, intAtt, new AttributeInfo(OWLClassM.getIntAttributeField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(etMock, longAtt, new AttributeInfo(OWLClassM.getLongAttributeField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(etMock, doubleAtt, new AttributeInfo(OWLClassM.getDoubleAttributeField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(etMock, dateAtt, new AttributeInfo(OWLClassM.getDateAttributeField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(etMock, characterAtt, new AttributeInfo(OWLClassM.getCharacterAttributeField(), Attribute.PersistentAttributeType.DATA).converter(new CharacterConverter())
                                                                                                                                             .language(null));
        initAttribute(etMock, enumAtt, new AttributeInfo(OWLClassM.getEnumAttributeField(), Attribute.PersistentAttributeType.DATA).converter(new StringEnumConverter<>(OWLClassM.Severity.class))
                                                                                                                                   .language(Generators.LANG));
        initAttribute(etMock, ordinalEnumAtt, new AttributeInfo(OWLClassM.getOrdinalEnumAttributeField(), Attribute.PersistentAttributeType.DATA).converter(new OrdinalEnumConverter(OWLClassM.Severity.class)));
        initAttribute(etMock, lexicalFormAtt, new AttributeInfo(OWLClassM.getLexicalFormField(), Attribute.PersistentAttributeType.DATA).converter(new ToLexicalFormConverter()));
        initAttribute(etMock, simpleLiteralAtt, new AttributeInfo(OWLClassM.getSimpleLiteralField(), Attribute.PersistentAttributeType.DATA).converter(new ToLexicalFormConverter())
                                                                                                                                            .language(null));
        initAttribute(etMock, explicitDatatypeAtt, new AttributeInfo(OWLClassM.getExplicitDatatypeField(), Attribute.PersistentAttributeType.DATA).converter(new ToLexicalFormConverter()));
        initAttribute(etMock, mWithConverterAtt, new AttributeInfo(OWLClassM.getWithConverterField(), Attribute.PersistentAttributeType.DATA).converter(
                new CustomConverterWrapper(new ZoneOffsetConverter(), String.class)));
        initAttribute(etMock, mObjectOneOfEnumAttribute, new AttributeInfo(OWLClassM.getObjectOneOfEnumAttributeField(), Attribute.PersistentAttributeType.OBJECT).converter(new ObjectOneOfEnumConverter(OneOfEnum.class)));
        initAttribute(etMock, intSetAtt, new AttributeInfo(OWLClassM.getIntegerSetField(), Attribute.PersistentAttributeType.DATA).collectionType(CollectionType.SET)
                                                                                                                                  .elementType(Integer.class)
                                                                                                                                  .valueType(BasicTypeImpl.get(Integer.class))
                                                                                                                                  .converter(new ToIntegerConverter()));
    }

    public static void initOWLClassNMock(IdentifiableEntityType<OWLClassN> et, SingularAttributeImpl annotationAtt,
                                         SingularAttributeImpl annotationUriAtt, SingularAttributeImpl stringAtt,
                                         AbstractPluralAttribute pluralAnnotationAtt,
                                         PropertiesSpecification props, Identifier idN)
            throws Exception {
        initEntityType(et, OWLClassN.class, EntityLifecycleListenerManager.empty());
        initIdentifier(et, idN, OWLClassN.getUriField(), false);
        when(et.getFieldSpecifications()).thenReturn(Set.of(annotationAtt, annotationUriAtt, stringAtt, pluralAnnotationAtt, props, idN));
        when(et.getAttributes()).thenReturn(Set.of(annotationAtt, annotationUriAtt, stringAtt, pluralAnnotationAtt));

        initAttribute(et, annotationAtt, new AttributeInfo(OWLClassN.getAnnotationPropertyField(), Attribute.PersistentAttributeType.ANNOTATION));
        initAttribute(et, annotationUriAtt, new AttributeInfo(OWLClassN.getAnnotationUriField(), Attribute.PersistentAttributeType.ANNOTATION));
        initAttribute(et, stringAtt, new AttributeInfo(OWLClassN.getStringAttributeField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(et, pluralAnnotationAtt,
                new AttributeInfo(OWLClassN.getPluralAnnotationField(), Attribute.PersistentAttributeType.ANNOTATION).elementType(String.class)
                                                                                                                     .collectionType(CollectionType.SET)
                                                                                                                     .valueType(BasicTypeImpl.get(String.class)));

        initProperties(et, props, new AttributeInfo(OWLClassN.getPropertiesField(), null), SingularAttribute.class, String.class);
    }

    public static void initOWLClassOMock(IdentifiableEntityType<OWLClassO> et, SingularAttributeImpl stringAtt,
                                         Identifier idO)
            throws Exception {
        initEntityType(et, OWLClassO.class, EntityLifecycleListenerManager.empty());
        initIdentifier(et, idO, OWLClassO.getUriField(), false);
        when(et.getAttributes()).thenReturn(Collections.singleton(stringAtt));
        when(et.getFieldSpecifications()).thenReturn(Set.of(stringAtt, idO));
        initAttribute(et, stringAtt, new AttributeInfo(OWLClassO.getStringAttributeField(), Attribute.PersistentAttributeType.DATA));
        when(et.getFieldSpecification(anyString())).thenThrow(IllegalArgumentException.class);
    }

    public static void initOWLClassPMock(IdentifiableEntityType<OWLClassP> et, TypesSpecification types,
                                         PropertiesSpecification props,
                                         SingularAttributeImpl uriAtt, AbstractPluralAttribute urlsAtt,
                                         ListAttributeImpl simpleListAtt, ListAttributeImpl refListAtt,
                                         Identifier idP) throws
            Exception {
        initEntityType(et, OWLClassP.class, EntityLifecycleListenerManager.empty());
        initIdentifier(et, idP, OWLClassP.getUriField(), false);
        when(et.getFieldSpecifications()).thenReturn(Set.of(uriAtt, urlsAtt, simpleListAtt, refListAtt, props, types, idP));
        when(et.getAttributes()).thenReturn(Set.of(uriAtt, urlsAtt, simpleListAtt, refListAtt));
        initProperties(et, props, new AttributeInfo(OWLClassP.getPropertiesField(), null), URI.class, Object.class);
        initTypesAttribute(et, types, new AttributeInfo(OWLClassP.getTypesField(), null).elementType(URI.class));
        initAttribute(et, uriAtt, new AttributeInfo(OWLClassP.getIndividualUriField(), Attribute.PersistentAttributeType.OBJECT));
        initAttribute(et, urlsAtt, new AttributeInfo(OWLClassP.getIndividualUrlsField(), Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.SET)
                                                                                                                                  .elementType(URL.class));
        initListAttribute(et, simpleListAtt, new AttributeInfo(OWLClassP.getSimpleListField(), Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.LIST)
                                                                                                                                        .elementType(URI.class)
                                                                                                                                        .valueType(BasicTypeImpl.get(URI.class)));
        initListAttribute(et, refListAtt, new AttributeInfo(OWLClassP.getReferencedListField(), Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.LIST)
                                                                                                                                         .elementType(URI.class)
                                                                                                                                         .valueType(BasicTypeImpl.get(URI.class)));
    }

    public static void initOwlClassQMock(IdentifiableEntityType<OWLClassQ> et,
                                         MappedSuperclassTypeImpl<QMappedSuperclass> superclassType,
                                         SingularAttributeImpl qStringAtt, SingularAttributeImpl qParentStringAtt,
                                         SingularAttributeImpl qLabelAtt, SingularAttributeImpl qOwlClassAAtt,
                                         Identifier idQ)
            throws Exception {
        initEntityType(et, OWLClassQ.class, EntityLifecycleListenerManager.empty());
        initIdentifier(et, idQ, OWLClassQ.getUriField(), false);
        when(et.getSupertypes()).thenReturn(Collections.singleton(superclassType));
        when(superclassType.getSubtypes()).thenReturn(Collections.singleton(et));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(et.getFieldSpecifications()).thenReturn(Set.of(qStringAtt, qParentStringAtt, qLabelAtt, qOwlClassAAtt, idQ));
        when(et.getAttributes()).thenReturn(Set.of(qStringAtt, qParentStringAtt, qLabelAtt, qOwlClassAAtt));

        initAttribute(et, qStringAtt, new AttributeInfo(OWLClassQ.getStringAttributeField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(et, qParentStringAtt, new AttributeInfo(OWLClassQ.getParentStringField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(et, qLabelAtt, new AttributeInfo(OWLClassQ.getLabelField(), Attribute.PersistentAttributeType.ANNOTATION));
        initAttribute(et, qOwlClassAAtt, new AttributeInfo(OWLClassQ.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT));
        when(et.getDeclaredAttribute(anyString())).thenAnswer(arg -> {
            if (Objects.equals(arg.getArgument(0), qStringAtt.getName())) {
                return qStringAtt;
            }
            throw new IllegalArgumentException();
        });
        initQMappedSuperclassMock(superclassType, qParentStringAtt, qLabelAtt, qOwlClassAAtt, idQ);
    }

    private static void initQMappedSuperclassMock(MappedSuperclassTypeImpl<QMappedSuperclass> superclassType,
                                                  SingularAttributeImpl qParentStringAtt,
                                                  SingularAttributeImpl qLabelAtt,
                                                  SingularAttributeImpl qOwlClassAAtt, Identifier idQ) {
        when(superclassType.getJavaType()).thenReturn(QMappedSuperclass.class);
        when(superclassType.getDeclaredAttribute(qParentStringAtt.getName())).thenReturn(qParentStringAtt);
        when(superclassType.getDeclaredAttribute(qLabelAtt.getName())).thenReturn(qLabelAtt);
        when(superclassType.getDeclaredAttribute(qOwlClassAAtt.getName())).thenReturn(qOwlClassAAtt);
        when(superclassType.getIdentifier()).thenReturn(idQ);
        doReturn(Set.of(qLabelAtt, qParentStringAtt, qOwlClassAAtt)).when(superclassType).getDeclaredAttributes();
    }

    public static void initOwlClassSMock(IdentifiableEntityType<OWLClassS> et, SingularAttributeImpl sNameAtt,
                                         TypesSpecification sTypes, Identifier idS) throws Exception {
        final EntityLifecycleListenerManager listenerManager = new EntityLifecycleListenerManager();
        addLifecycleCallback(listenerManager, PRE_PERSIST, OWLClassS.getPrePersistHook());
        when(et.getLifecycleListenerManager()).thenReturn(listenerManager);
        initEntityType(et, OWLClassS.class, listenerManager);
        when(et.getFieldSpecifications()).thenReturn(Set.of(sNameAtt, sTypes, idS));
        when(et.getAttributes()).thenReturn(Collections.singleton(sNameAtt));
        when(et.isAbstract()).thenReturn(true);
        initIdentifier(et, idS, OWLClassS.getUriField(), true);

        initAttribute(et, sNameAtt, new AttributeInfo(OWLClassS.getNameField(), Attribute.PersistentAttributeType.ANNOTATION));
        initTypesAttribute(et, sTypes, new AttributeInfo(OWLClassS.getTypesField(), null).elementType(String.class));
    }

    static void initOwlClassRMock(IdentifiableEntityType<OWLClassR> et, SingularAttributeImpl rStringAtt,
                                  SingularAttributeImpl owlClassAAtt, IdentifiableEntityType<OWLClassS> parentEt)
            throws Exception {
        final EntityLifecycleListenerManager listenerManager = new EntityLifecycleListenerManager();
        final Method addParent = EntityLifecycleListenerManager.class
                .getDeclaredMethod("addParent", EntityLifecycleListenerManager.class);
        addParent.setAccessible(true);
        addParent.invoke(listenerManager, parentEt.getLifecycleListenerManager());
        addLifecycleCallback(listenerManager, PRE_PERSIST, OWLClassR.getPrePersistHook());
        addLifecycleCallback(listenerManager, POST_PERSIST, OWLClassR.getPostPersistHook());
        addLifecycleCallback(listenerManager, PRE_UPDATE, OWLClassR.getPreUpdateHook());
        addLifecycleCallback(listenerManager, POST_UPDATE, OWLClassR.getPostUpdateHook());
        addLifecycleCallback(listenerManager, PRE_REMOVE, OWLClassR.getPreRemoveHook());
        addLifecycleCallback(listenerManager, POST_REMOVE, OWLClassR.getPostRemoveHook());
        addLifecycleCallback(listenerManager, POST_LOAD, OWLClassR.getPostLoadHook());
        when(et.getLifecycleListenerManager()).thenReturn(listenerManager);

        initEntityType(et, OWLClassR.class, listenerManager);
        final Identifier id = parentEt.getIdentifier();
        when(et.getIdentifier()).thenReturn(id);
        final Set attributes = new HashSet<>(parentEt.getAttributes());
        attributes.add(rStringAtt);
        attributes.add(owlClassAAtt);
        final Set fieldSpecs = new HashSet(parentEt.getFieldSpecifications());
        fieldSpecs.add(rStringAtt);
        fieldSpecs.add(owlClassAAtt);
        when(et.getFieldSpecifications()).thenReturn(fieldSpecs);
        when(et.getAttributes()).thenReturn(attributes);
        when(et.getSupertypes()).thenReturn(Collections.singleton(parentEt));
        when(parentEt.getSubtypes()).thenReturn(Collections.singleton(et));
        when(parentEt.hasSubtypes()).thenReturn(true);

        initAttribute(et, rStringAtt, new AttributeInfo(OWLClassR.getStringAttField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(et, owlClassAAtt, new AttributeInfo(OWLClassR.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT));

        for (Attribute att : parentEt.getAttributes()) {
            when(et.getAttribute(att.getName())).thenReturn((AbstractAttribute) att);
        }
        for (FieldSpecification fs : parentEt.getFieldSpecifications()) {
            when(et.getFieldSpecification(fs.getName())).thenReturn(fs);
        }
    }

    static void initOwlClassSListeners(IdentifiableEntityType<OWLClassS> etS,
                                       ParentListener listener) throws Exception {
        final EntityLifecycleListenerManager manager = etS.getLifecycleListenerManager();
        final Method addListener = EntityLifecycleListenerManager.class
                .getDeclaredMethod("addEntityListener", Object.class);
        addListener.setAccessible(true);
        addListener.invoke(manager, listener);
        addEntityListenerCallback(manager, listener, PRE_PERSIST, ParentListener.getPrePersistMethod());
        addEntityListenerCallback(manager, listener, POST_PERSIST, ParentListener.getPostPersistMethod());
    }

    private static void addEntityListenerCallback(EntityLifecycleListenerManager manager, Object listener,
                                                  LifecycleEvent evt, Method callback)
            throws Exception {
        final Method addListenerCallback = EntityLifecycleListenerManager.class
                .getDeclaredMethod("addEntityListenerCallback", Object.class, LifecycleEvent.class, Method.class);
        if (!addListenerCallback.canAccess(manager)) {
            addListenerCallback.setAccessible(true);
        }
        addListenerCallback.invoke(manager, listener, evt, callback);
    }

    static void initOwlClassRListeners(IdentifiableEntityType<OWLClassR> etR, IdentifiableEntityType<OWLClassS> etS,
                                       ConcreteListener concreteListener, AnotherListener anotherListener)
            throws Exception {
        final EntityLifecycleListenerManager manager = etR.getLifecycleListenerManager();
        final Method addParent = EntityLifecycleListenerManager.class
                .getDeclaredMethod("addParent", EntityLifecycleListenerManager.class);
        addParent.setAccessible(true);
        addParent.invoke(manager, etS.getLifecycleListenerManager());
        final Method addListener = EntityLifecycleListenerManager.class
                .getDeclaredMethod("addEntityListener", Object.class);
        addListener.setAccessible(true);
        addListener.invoke(manager, concreteListener);
        addListener.invoke(manager, anotherListener);
        addEntityListenerCallback(manager, concreteListener, PRE_PERSIST, ConcreteListener.getPrePersist());
        addEntityListenerCallback(manager, anotherListener, PRE_PERSIST, AnotherListener.getPrePersist());
        addEntityListenerCallback(manager, concreteListener, POST_PERSIST, ConcreteListener.getPostPersist());
        addEntityListenerCallback(manager, concreteListener, POST_LOAD, ConcreteListener.getPostLoad());
        addEntityListenerCallback(manager, concreteListener, PRE_UPDATE, ConcreteListener.getPreUpdate());
        addEntityListenerCallback(manager, concreteListener, POST_UPDATE, ConcreteListener.getPostUpdate());
        addEntityListenerCallback(manager, concreteListener, PRE_REMOVE, ConcreteListener.getPreRemove());
        addEntityListenerCallback(manager, concreteListener, POST_REMOVE, ConcreteListener.getPostRemove());
    }

    static void initOwlClassTMock(IdentifiableEntityType<OWLClassT> et, SingularAttributeImpl localDateAtt,
                                  SingularAttributeImpl localDateTimeAtt, SingularAttributeImpl owlClassSAtt,
                                  IdentifiableEntityType<OWLClassS> etS,
                                  Identifier id) throws Exception {
        initEntityType(et, OWLClassT.class, etS.getLifecycleListenerManager());
        initIdentifier(et, id, OWLClassT.getUriField(), true);
        when(et.getFieldSpecifications()).thenReturn(Set.of(localDateAtt, localDateTimeAtt, id));
        when(et.getAttributes()).thenReturn(Set.of(localDateAtt, localDateTimeAtt));

        initAttribute(et, localDateAtt, new AttributeInfo(OWLClassT.getLocalDateField(), Attribute.PersistentAttributeType.DATA)
                .converter(DefaultConverterWrapper.INSTANCE));
        initAttribute(et, localDateTimeAtt, new AttributeInfo(OWLClassT.getLocalDateTimeField(), Attribute.PersistentAttributeType.DATA)
                .converter(new LocalDateTimeConverter()));
        initAttribute(et, owlClassSAtt, new AttributeInfo(OWLClassT.getOwlClassSField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etS));
    }

    static void initOwlClassUMocks(IdentifiableEntityType<OWLClassU> et, SingularAttributeImpl singularStringAtt,
                                   AbstractPluralAttribute pluralStringAtt, SingularAttributeImpl modified,
                                   Identifier id) throws Exception {
        final EntityLifecycleListenerManager listenerManager = new EntityLifecycleListenerManager();
        addLifecycleCallback(listenerManager, PRE_UPDATE, OWLClassU.class.getDeclaredMethod("preUpdate"));
        when(et.getLifecycleListenerManager()).thenReturn(listenerManager);
        initEntityType(et, OWLClassU.class, listenerManager);
        initIdentifier(et, id, OWLClassU.getIdField(), true);
        when(et.getFieldSpecifications()).thenReturn(Set.of(singularStringAtt, pluralStringAtt, modified, id));
        when(et.getAttributes()).thenReturn(Set.of(singularStringAtt, pluralStringAtt, modified));

        initAttribute(et, singularStringAtt, new AttributeInfo(OWLClassU.getSingularStringAttField(), Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(et, modified, new AttributeInfo(OWLClassU.getModifiedField(), Attribute.PersistentAttributeType.DATA).converter(new LocalDateTimeConverter())
                                                                                                                           .language(null));
        initAttribute(et, pluralStringAtt, new AttributeInfo(OWLClassU.getPluralStringAttField(), Attribute.PersistentAttributeType.DATA).collectionType(CollectionType.SET)
                                                                                                                                         .elementType(MultilingualString.class)
                                                                                                                                         .language(null));
    }

    static void initOwlClassWMocks(IdentifiableEntityType<OWLClassW> et, AbstractPluralAttribute setStringAtt,
                                   ListAttributeImpl listStringAtt, AbstractPluralAttribute collectionStringAtt,
                                   AbstractQueryAttribute setQueryStringAtt, AbstractQueryAttribute listQueryStringAtt,
                                   Identifier id) throws Exception {
        initEntityType(et, OWLClassW.class, EntityLifecycleListenerManager.empty());
        initIdentifier(et, id, OWLClassW.getIdField(), true);
        when(et.getFieldSpecifications())
                .thenReturn(Set.of(setStringAtt, listStringAtt, collectionStringAtt, setQueryStringAtt, listQueryStringAtt, id));
        when(et.getAttributes()).thenReturn((Set) Set.of(setStringAtt, listStringAtt, collectionStringAtt, setQueryStringAtt, listQueryStringAtt));

        initAttribute(et, setStringAtt,
                new AttributeInfo(OWLClassW.getSetStringAttField(), Attribute.PersistentAttributeType.DATA).collectionType(CollectionType.SET)
                                                                                                           .elementType(MultilingualString.class)
                                                                                                           .language(null));
        initAttribute(et, collectionStringAtt, new AttributeInfo(OWLClassW.getCollectionStringAttField(), Attribute.PersistentAttributeType.DATA).collectionType(CollectionType.COLLECTION)
                                                                                                                                                 .elementType(MultilingualString.class)
                                                                                                                                                 .language(null));
        initListAttribute(et, listStringAtt, new AttributeInfo(OWLClassW.getListStringAttField(), Attribute.PersistentAttributeType.DATA).collectionType(CollectionType.LIST)
                                                                                                                                         .elementType(String.class)
                                                                                                                                         .valueType(BasicTypeImpl.get(String.class))
                                                                                                                                         .language(null));

        when(setQueryStringAtt.getJavaField()).thenReturn(OWLClassW.getSetQueryStringAttField());
        when(setQueryStringAtt.getJavaType()).thenReturn(OWLClassW.getSetQueryStringAttField().getType());
        when(setQueryStringAtt.getName()).thenReturn(OWLClassW.getSetQueryStringAttField().getName());
        when(setQueryStringAtt.getDeclaringType()).thenReturn(et);
        when(setQueryStringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(setQueryStringAtt.getQuery()).thenReturn(OWLClassW.getSetQueryStringAttField().getAnnotation(Sparql.class)
                                                               .query());
        when(setQueryStringAtt.enableReferencingAttributes()).thenReturn(true);
        when(et.getFieldSpecification(OWLClassW.getSetQueryStringAttField().getName())).thenReturn(setQueryStringAtt);

        when(listQueryStringAtt.getJavaField()).thenReturn(OWLClassW.getListQueryStringAttField());
        when(listQueryStringAtt.getJavaType()).thenReturn(OWLClassW.getListQueryStringAttField().getType());
        when(listQueryStringAtt.getName()).thenReturn(OWLClassW.getListQueryStringAttField().getName());
        when(listQueryStringAtt.getDeclaringType()).thenReturn(et);
        when(listQueryStringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(listQueryStringAtt.getQuery()).thenReturn(OWLClassW.getListQueryStringAttField()
                                                                .getAnnotation(Sparql.class).query());
        when(listQueryStringAtt.enableReferencingAttributes()).thenReturn(true);
        when(et.getFieldSpecification(OWLClassW.getListQueryStringAttField().getName())).thenReturn(listQueryStringAtt);
    }

    static void initOWLClassWithQueryAttrMocks(IdentifiableEntityType<OWLClassWithQueryAttr> etMock,
                                               AbstractQueryAttribute strQueryAttMock, SingularAttributeImpl strAttMock,
                                               AbstractQueryAttribute entityQueryAttMock,
                                               SingularAttributeImpl entityAttMock,
                                               IdentifiableEntityType<OWLClassA> etAMock,
                                               Identifier idMock) throws NoSuchFieldException {
        initEntityType(etMock, OWLClassWithQueryAttr.class, EntityLifecycleListenerManager.empty());
        when(etMock.getQueryAttribute(OWLClassWithQueryAttr.getStrQueryAttField()
                                                           .getName())).thenReturn(strQueryAttMock);

        when(etMock.getAttributes()).thenReturn(Set.of(strAttMock, entityAttMock));
        when(etMock.getQueryAttributes()).thenReturn(Set.of(strQueryAttMock, entityQueryAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(strAttMock, strQueryAttMock, entityAttMock, entityQueryAttMock, idMock));

        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassWithQueryAttr.getStrAttField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(etMock, entityAttMock, new AttributeInfo(OWLClassWithQueryAttr.getEntityAttField(), Attribute.PersistentAttributeType.OBJECT).valueType(etAMock));

        when(etMock.getFieldSpecification(strQueryAttMock.getName())).thenReturn(strQueryAttMock);
        when(etMock.getFieldSpecification(entityQueryAttMock.getName())).thenReturn(entityQueryAttMock);

        when(strQueryAttMock.getJavaField()).thenReturn(OWLClassWithQueryAttr.getStrQueryAttField());
        when(strQueryAttMock.getJavaType()).thenReturn(OWLClassWithQueryAttr.getStrQueryAttField().getType());
        when(strQueryAttMock.getName()).thenReturn(OWLClassWithQueryAttr.getStrQueryAttField().getName());
        when(strQueryAttMock.getDeclaringType()).thenReturn(etMock);
        when(strQueryAttMock.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(strQueryAttMock.getQuery()).thenReturn(
                OWLClassWithQueryAttr.getStrQueryAttField().getAnnotation(Sparql.class).query());
        when(strQueryAttMock.enableReferencingAttributes()).thenReturn(true);

        when(entityQueryAttMock.getJavaField()).thenReturn(OWLClassWithQueryAttr.getEntityQueryAttField());
        when(entityQueryAttMock.getJavaType()).thenReturn(OWLClassWithQueryAttr.getEntityQueryAttField().getType());
        when(entityQueryAttMock.getName()).thenReturn(OWLClassWithQueryAttr.getEntityQueryAttField().getName());
        when(entityQueryAttMock.getDeclaringType()).thenReturn(etMock);
        when(entityQueryAttMock.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(entityQueryAttMock.getQuery()).thenReturn(
                OWLClassWithQueryAttr.getEntityQueryAttField().getAnnotation(Sparql.class).query());
        when(entityQueryAttMock.enableReferencingAttributes()).thenReturn(true);

        initIdentifier(etMock, idMock, OWLClassWithQueryAttr.class.getDeclaredField("uri"), false);
    }

    public static void initPhoneMocks(IdentifiableEntityType<Phone> etMock, SingularAttributeImpl phoneNumberAttMock,
                                      SingularAttributeImpl phoneBrandAttMock,
                                      Identifier idMock) throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, Phone.class, EntityLifecycleListenerManager.empty());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(phoneNumberAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(phoneNumberAttMock, idMock));
        initAttribute(etMock, phoneNumberAttMock, new AttributeInfo(Phone.class.getDeclaredField("number"), Attribute.PersistentAttributeType.DATA));
        initAttribute(etMock, phoneBrandAttMock, new AttributeInfo(Phone.class.getDeclaredField("brand"), Attribute.PersistentAttributeType.DATA));
        initIdentifier(etMock, idMock, Phone.class.getDeclaredField("uri"), false);
    }

    public static void initPersonMocks(IdentifiableEntityType<Person> etMock,
                                       SingularAttributeImpl firstNameAttMock,
                                       SingularAttributeImpl lastNameAttMock,
                                       SingularAttributeImpl usernameAttMock,
                                       SingularAttributeImpl genderAttMock,
                                       SingularAttributeImpl ageAttMock, SingularAttributeImpl phoneAttMock,
                                       AbstractIdentifiableType<Phone> etPhone,
                                       TypesSpecification typesMock,
                                       Identifier idMock) throws NoSuchFieldException, SecurityException {
        initEntityType(etMock, Person.class, EntityLifecycleListenerManager.empty());
        initIdentifier(etMock, idMock, Person.class.getDeclaredField("uri"), false);
        when(etMock.getAttributes()).thenReturn(Set.of(usernameAttMock, genderAttMock, ageAttMock, phoneAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(usernameAttMock, genderAttMock, ageAttMock, phoneAttMock, idMock));

        initAttribute(etMock, firstNameAttMock, new AttributeInfo(Person.class.getDeclaredField("firstName"),
                Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(etMock, lastNameAttMock, new AttributeInfo(Person.class.getDeclaredField("lastName"),
                Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(etMock, usernameAttMock, new AttributeInfo(Person.class.getDeclaredField("username"),
                Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(etMock, genderAttMock, new AttributeInfo(Person.class.getDeclaredField("gender"),
                Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(etMock, ageAttMock, new AttributeInfo(Person.class.getDeclaredField("age"),
                Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(etMock, phoneAttMock, new AttributeInfo(Person.class.getDeclaredField("phone"),
                Attribute.PersistentAttributeType.OBJECT).valueType(etPhone));

        initTypesAttribute(etMock, typesMock, new AttributeInfo(Person.class.getDeclaredField("types"), null)
                .elementType(String.class));
    }

    private static class AttributeInfo<V> {
        private final Field field;
        private final Attribute.PersistentAttributeType type;
        private ParticipationConstraint[] constraints = new ParticipationConstraint[0];
        private boolean nonEmpty;
        private String language = Generators.LANG;
        private Type<V> valueType;
        private Class<V> elementType;
        private ConverterWrapper converter;
        private CollectionType collectionType;

        private AttributeInfo(Field field, Attribute.PersistentAttributeType type) {
            this.field = field;
            this.type = type;
        }

        private AttributeInfo constraints(ParticipationConstraint... constraints) {
            this.constraints = constraints;
            return this;
        }

        private AttributeInfo nonEmpty() {
            this.nonEmpty = true;
            return this;
        }

        private AttributeInfo language(String language) {
            this.language = language;
            return this;
        }

        private AttributeInfo valueType(Type<V> valueType) {
            this.valueType = valueType;
            return this;
        }

        private AttributeInfo elementType(Class<V> elementType) {
            this.elementType = elementType;
            return this;
        }

        private AttributeInfo converter(ConverterWrapper converter) {
            this.converter = converter;
            return this;
        }

        private AttributeInfo collectionType(CollectionType collectionType) {
            this.collectionType = collectionType;
            return this;
        }
    }
}
