/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.Vocabulary;
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
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
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
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.MappedSuperclassTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
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
        when(etMock.getJavaType()).thenReturn(OWLClassA.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassA.class));
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassA.getClassIri()));
        when(etMock.getAttribute(OWLClassA.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getName()).thenReturn(OWLClassA.class.getSimpleName());
        when(etMock.getTypes()).thenReturn(typesMock);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(strAttMock, typesMock, idMock));

        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassA.getStrAttField(), Attribute.PersistentAttributeType.DATA)
                .language(Generators.LANG));
        when(typesMock.getJavaField()).thenReturn(OWLClassA.getTypesField());
        when(typesMock.getName()).thenReturn(OWLClassA.getTypesField().getName());
        when(typesMock.getDeclaringType()).thenReturn(etMock);
        when(typesMock.getJavaType()).thenReturn(Set.class);
        when(typesMock.getElementType()).thenReturn(String.class);
        when(typesMock.isCollection()).thenReturn(true);
        when(typesMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(etMock.getFieldSpecification(typesMock.getName())).thenReturn(typesMock);

        initIdentifier(etMock, idMock, OWLClassA.class.getDeclaredField("uri"), false);
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
    }

    private static <X> void initIdentifier(IdentifiableEntityType<X> et, Identifier id, Field idField,
                                           boolean generated) {
        when(et.getIdentifier()).thenReturn(id);
        when(id.getJavaField()).thenReturn(idField);
        when(id.getDeclaringType()).thenReturn(et);
        when(id.getName()).thenReturn(idField.getName());
        when(id.isGenerated()).thenReturn(generated);
        when(et.getFieldSpecification(idField.getName())).thenReturn(id);
    }

    private static <X> void initAttribute(IdentifiableEntityType<X> etMock, AbstractAttribute attMock,
                                          AttributeInfo attInfo) {
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
        when(attMock.isCollection()).thenReturn(Collection.class.isAssignableFrom(attInfo.field.getType()));
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

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassBMocks(IdentifiableEntityType<OWLClassB> etMock, SingularAttributeImpl strAttMock,
                                          PropertiesSpecification propsMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassB.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassB.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassB.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassB.class.getSimpleName());
        when(etMock.getAttribute(OWLClassB.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getProperties()).thenReturn(propsMock);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(strAttMock, propsMock, idMock));

        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassB.getStrAttField(), Attribute.PersistentAttributeType.DATA)
                .language(Generators.LANG));
        when(etMock.getFieldSpecification(strAttMock.getName())).thenReturn(strAttMock);
        when(propsMock.getJavaField()).thenReturn(OWLClassB.getPropertiesField());
        when(propsMock.getJavaType()).thenReturn(OWLClassB.getPropertiesField().getType());
        when(propsMock.getName()).thenReturn(OWLClassB.getPropertiesField().getName());
        when(propsMock.getDeclaringType()).thenReturn(etMock);
        when(propsMock.getPropertyIdentifierType()).thenReturn(String.class);
        when(propsMock.getPropertyValueType()).thenReturn(String.class);
        when(etMock.getFieldSpecification(propsMock.getName())).thenReturn(propsMock);

        initIdentifier(etMock, idMock, OWLClassB.class.getDeclaredField("uri"), false);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
        when(etMock.getDeclaredAttribute(anyString())).thenAnswer(args -> {
            final String name = args.getArgument(0);
            if (Objects.equals(name, strAttMock.getName())) {
                return strAttMock;
            }
            throw new IllegalArgumentException("Unknown attribute " + name);
        });
    }

    public static void initOWLClassCMocks(IdentifiableEntityType<OWLClassC> etMock,
                                          ListAttributeImpl simpleListMock, ListAttributeImpl refListMock,
                                          RdfContainerAttributeImpl rdfSeqMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassC.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassC.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassC.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassC.class.getSimpleName());
        when(etMock.getAttribute(OWLClassC.getSimpleListField().getName())).thenReturn(simpleListMock);
        when(etMock.getAttribute(OWLClassC.getRefListField().getName())).thenReturn(refListMock);
        when(etMock.getAttribute(OWLClassC.getRdfSeqField().getName())).thenReturn(rdfSeqMock);
        when(etMock.getAttributes()).thenReturn(Set.of(simpleListMock, refListMock, rdfSeqMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(simpleListMock, refListMock, rdfSeqMock, idMock));
        when(simpleListMock.getJavaField()).thenReturn(OWLClassC.getSimpleListField());
        when(refListMock.getJavaField()).thenReturn(OWLClassC.getRefListField());
        when(rdfSeqMock.getJavaField()).thenReturn(OWLClassC.getRdfSeqField());
        String attIri = OWLClassC.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri();
        when(simpleListMock.getIRI()).thenReturn(IRI.create(attIri));
        when(simpleListMock.getName()).thenReturn(OWLClassC.getSimpleListField().getName());
        when(etMock.getFieldSpecification(simpleListMock.getName())).thenReturn(simpleListMock);
        String hasListAttIri = OWLClassC.getSimpleListField().getAnnotation(Sequence.class).listClassIRI();
        when(simpleListMock.getSequenceType()).thenReturn(SequenceType.simple);
        when(simpleListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(simpleListMock.getListClassIRI()).thenReturn(IRI.create(hasListAttIri));
        String hasNextIri = OWLClassC.getSimpleListField().getAnnotation(Sequence.class).hasNextPropertyIRI();
        when(simpleListMock.getHasNextPropertyIRI()).thenReturn(IRI.create(hasNextIri));
        when(simpleListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(simpleListMock.getPersistentAttributeType())
                .thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(simpleListMock.isCollection()).thenReturn(Boolean.TRUE);
        when(simpleListMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(simpleListMock.isAssociation()).thenReturn(true);
        when(simpleListMock.getDeclaringType()).thenReturn(etMock);
        when(simpleListMock.getJavaType()).thenReturn(List.class);
        when(simpleListMock.getFetchType()).thenReturn(FetchType.LAZY);
        when(simpleListMock.getCascadeTypes())
                .thenReturn(OWLClassC.getSimpleListField().getAnnotation(OWLObjectProperty.class).cascade());

        hasListAttIri = OWLClassC.getRefListField().getAnnotation(Sequence.class).listClassIRI();
        when(refListMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(refListMock.getSequenceType()).thenReturn(SequenceType.referenced);
        when(refListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(refListMock.getListClassIRI()).thenReturn(IRI.create(hasListAttIri));
        when(refListMock.getName()).thenReturn(OWLClassC.getRefListField().getName());
        when(etMock.getFieldSpecification(refListMock.getName())).thenReturn(refListMock);
        hasNextIri = OWLClassC.getRefListField().getAnnotation(Sequence.class).hasNextPropertyIRI();
        when(refListMock.getHasNextPropertyIRI()).thenReturn(IRI.create(hasNextIri));
        final String contentIri = OWLClassC.getRefListField().getAnnotation(Sequence.class).hasContentsPropertyIRI();
        when(refListMock.getHasContentsPropertyIRI()).thenReturn(IRI.create(contentIri));
        attIri = OWLClassC.getRefListField().getAnnotation(OWLObjectProperty.class).iri();
        when(refListMock.getIRI()).thenReturn(IRI.create(attIri));
        when(refListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(refListMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(refListMock.isCollection()).thenReturn(Boolean.TRUE);
        when(refListMock.isAssociation()).thenReturn(true);
        when(refListMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(refListMock.getDeclaringType()).thenReturn(etMock);
        when(refListMock.getJavaType()).thenReturn(List.class);
        when(refListMock.getCascadeTypes())
                .thenReturn(OWLClassC.getRefListField().getAnnotation(OWLObjectProperty.class).cascade());

        when(rdfSeqMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(rdfSeqMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(rdfSeqMock.getContainerType()).thenReturn(RDFContainerType.SEQ);
        when(rdfSeqMock.getName()).thenReturn(OWLClassC.getRdfSeqField().getName());
        when(etMock.getFieldSpecification(rdfSeqMock.getName())).thenReturn(rdfSeqMock);
        when(rdfSeqMock.getIRI()).thenReturn(IRI.create(OWLClassC.getRdfSeqField()
                                                                 .getAnnotation(OWLObjectProperty.class).iri()));
        when(rdfSeqMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(rdfSeqMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(rdfSeqMock.isCollection()).thenReturn(Boolean.TRUE);
        when(rdfSeqMock.isAssociation()).thenReturn(true);
        when(rdfSeqMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(rdfSeqMock.getDeclaringType()).thenReturn(etMock);
        when(rdfSeqMock.getJavaType()).thenReturn(List.class);
        when(rdfSeqMock.getCascadeTypes())
                .thenReturn(OWLClassC.getRdfSeqField().getAnnotation(OWLObjectProperty.class).cascade());


        initIdentifier(etMock, idMock, OWLClassC.class.getDeclaredField("uri"), false);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
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

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassDMocks(IdentifiableEntityType<OWLClassD> etMock, SingularAttributeImpl clsAMock,
                                          IdentifiableEntityType<OWLClassA> etA, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassD.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassD.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassD.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassD.class.getSimpleName());
        when(etMock.getAttribute(OWLClassD.getOwlClassAField().getName())).thenReturn(clsAMock);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(clsAMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(clsAMock, idMock));
        initAttribute(etMock, clsAMock, new AttributeInfo(OWLClassD.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT).valueType(etA));
        when(etMock.getFieldSpecification(clsAMock.getName())).thenReturn(clsAMock);
        initIdentifier(etMock, idMock, OWLClassD.getUriField(), false);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassEMocks(IdentifiableEntityType<OWLClassE> etMock, SingularAttributeImpl strAttMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassE.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassE.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassE.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassE.class.getSimpleName());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(strAttMock, idMock));
        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassE.getStrAttField(), Attribute.PersistentAttributeType.DATA)
                .language(Generators.LANG));
        initIdentifier(etMock, idMock, OWLClassE.class.getDeclaredField("uri"), true);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassFMocks(IdentifiableEntityType<OWLClassF> etMock, AbstractPluralAttribute setAMock,
                                          SingularAttributeImpl strAttMock, IdentifiableEntityType<OWLClassA> etAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassF.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassF.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassF.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassF.class.getSimpleName());
        when(etMock.getAttribute(OWLClassF.getSimpleSetField().getName())).thenReturn(setAMock);
        when(etMock.getAttributes()).thenReturn(Set.of(setAMock, strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(setAMock, strAttMock, idMock));
        initAttribute(etMock, setAMock, new AttributeInfo(OWLClassF.getSimpleSetField(),
                Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.SET)
                                                         .elementType(OWLClassA.class).valueType(etAMock));
        initAttribute(etMock, strAttMock, new AttributeInfo(OWLClassF.getStrAttField(), Attribute.PersistentAttributeType.DATA)
                .language(Generators.LANG));

        initIdentifier(etMock, idMock, OWLClassF.class.getDeclaredField("uri"), false);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassGMocks(IdentifiableEntityType<OWLClassG> etMock, SingularAttributeImpl clsHMock,
                                          IdentifiableEntityType<OWLClassH> etHMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassG.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassG.class));
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassG.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassG.class.getSimpleName());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(clsHMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(clsHMock, idMock));
        initAttribute(etMock, clsHMock, new AttributeInfo(OWLClassG.getOwlClassHField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etHMock));
        initIdentifier(etMock, idMock, OWLClassG.class.getDeclaredField("uri"), false);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassHMocks(IdentifiableEntityType<OWLClassH> etMock, SingularAttributeImpl clsAMock,
                                          SingularAttributeImpl clsGMock, IdentifiableEntityType<OWLClassA> etAMock,
                                          IdentifiableEntityType<OWLClassG> etGMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassH.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassH.class));
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassH.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassH.class.getSimpleName());
        when(etMock.getAttributes()).thenReturn(Set.of(clsAMock, clsGMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(clsAMock, clsGMock, idMock));

        initAttribute(etMock, clsAMock, new AttributeInfo(OWLClassH.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etAMock));
        initAttribute(etMock, clsGMock, new AttributeInfo(OWLClassH.getOwlClassGField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etGMock));
        initIdentifier(etMock, idMock, OWLClassH.class.getDeclaredField("uri"), false);

        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassIMocks(IdentifiableEntityType<OWLClassI> etMock, SingularAttributeImpl aAttMock,
                                          IdentifiableEntityType<OWLClassA> etAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassI.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassI.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassI.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassI.class.getSimpleName());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(aAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(aAttMock, idMock));

        initAttribute(etMock, aAttMock, new AttributeInfo(OWLClassI.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etAMock));
        initIdentifier(etMock, idMock, OWLClassI.class.getDeclaredField("uri"), false);

        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassJMocks(IdentifiableEntityType<OWLClassJ> etMock, AbstractPluralAttribute setAMock,
                                          IdentifiableEntityType<OWLClassA> etAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassJ.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassJ.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassJ.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassJ.class.getSimpleName());
        when(etMock.getAttribute(OWLClassJ.getOwlClassAField().getName())).thenReturn(setAMock);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(setAMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(setAMock, idMock));
        initAttribute(etMock, setAMock, new AttributeInfo(OWLClassJ.getOwlClassAField(),
                Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.SET)
                                                         .elementType(OWLClassA.class).valueType(etAMock));
        initIdentifier(etMock, idMock, OWLClassJ.class.getDeclaredField("uri"), false);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassKMocks(IdentifiableEntityType<OWLClassK> etMock, SingularAttributeImpl clsEMock,
                                          IdentifiableEntityType<OWLClassE> etEMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassK.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassK.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassK.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassK.class.getSimpleName());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(clsEMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(clsEMock, idMock));

        initAttribute(etMock, clsEMock, new AttributeInfo(OWLClassK.getOwlClassEField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etEMock));
        initIdentifier(etMock, idMock, OWLClassK.class.getDeclaredField("uri"), false);

        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassLMocks(IdentifiableEntityType<OWLClassL> etMock, ListAttributeImpl refListMock,
                                          ListAttributeImpl simpleListMock, AbstractPluralAttribute setMock,
                                          SingularAttributeImpl singleAMock, IdentifiableEntityType<OWLClassA> etAMock,
                                          Identifier idMock) throws NoSuchFieldException {
        when(etMock.getJavaType()).thenReturn(OWLClassL.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassL.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassL.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassL.class.getSimpleName());
        initIdentifier(etMock, idMock, OWLClassL.class.getDeclaredField("uri"), false);
        when(etMock.getDeclaredAttributes()).thenReturn(Set.of(refListMock, simpleListMock, setMock, singleAMock));
        when(etMock.getAttributes()).thenReturn(Set.of(refListMock, simpleListMock, setMock, singleAMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(refListMock, simpleListMock, setMock, singleAMock, idMock));

        when(refListMock.getJavaField()).thenReturn(OWLClassL.getReferencedListField());
        when(refListMock.getName()).thenReturn(OWLClassL.getReferencedListField().getName());
        when(refListMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(refListMock.getIRI()).thenReturn(
                IRI.create(OWLClassL.getReferencedListField().getAnnotation(OWLObjectProperty.class).iri()));
        when(refListMock.getConstraints()).thenReturn(
                OWLClassL.getReferencedListField().getAnnotation(ParticipationConstraints.class).value());
        when(refListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(refListMock.getSequenceType()).thenReturn(SequenceType.referenced);
        when(refListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(refListMock.isCollection()).thenReturn(true);
        when(refListMock.getHasNextPropertyIRI()).thenReturn(IRI.create(
                OWLClassL.getReferencedListField().getAnnotation(Sequence.class).hasNextPropertyIRI()));
        when(refListMock.getHasContentsPropertyIRI()).thenReturn(IRI.create(
                OWLClassL.getReferencedListField().getAnnotation(Sequence.class).hasContentsPropertyIRI()));
        when(etMock.getFieldSpecification(OWLClassL.getReferencedListField().getName())).thenReturn(refListMock);
        when(etMock.getAttribute(OWLClassL.getReferencedListField().getName())).thenReturn(refListMock);
        when(refListMock.getDeclaringType()).thenReturn(etMock);
        when(refListMock.getJavaType()).thenReturn(List.class);

        when(simpleListMock.getJavaField()).thenReturn(OWLClassL.getSimpleListField());
        when(simpleListMock.getName()).thenReturn(OWLClassL.getSimpleListField().getName());
        when(simpleListMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(simpleListMock.getIRI()).thenReturn(
                IRI.create(OWLClassL.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri()));
        when(simpleListMock.getConstraints()).thenReturn(
                OWLClassL.getSimpleListField().getAnnotation(ParticipationConstraints.class).value());
        when(simpleListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(simpleListMock.getSequenceType()).thenReturn(SequenceType.simple);
        when(simpleListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(simpleListMock.isCollection()).thenReturn(true);
        when(simpleListMock.getHasNextPropertyIRI()).thenReturn(
                IRI.create(OWLClassL.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri()));
        when(etMock.getFieldSpecification(OWLClassL.getSimpleListField().getName())).thenReturn(simpleListMock);
        when(etMock.getAttribute(OWLClassL.getSimpleListField().getName())).thenReturn(simpleListMock);
        when(simpleListMock.getDeclaringType()).thenReturn(etMock);
        when(simpleListMock.getJavaType()).thenReturn(List.class);

        initAttribute(etMock, setMock, new AttributeInfo(OWLClassL.getSetField(),
                Attribute.PersistentAttributeType.OBJECT).valueType(etAMock).elementType(OWLClassA.class)
                                                         .collectionType(CollectionType.SET));

        initAttribute(etMock, singleAMock, new AttributeInfo(OWLClassL.getSingleAField(), Attribute.PersistentAttributeType.OBJECT)
                .valueType(etAMock).nonEmpty());
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
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
        when(etMock.getJavaType()).thenReturn(OWLClassM.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassM.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassM.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassM.class.getSimpleName());
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

        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassNMock(IdentifiableEntityType<OWLClassN> et, SingularAttributeImpl annotationAtt,
                                         SingularAttributeImpl annotationUriAtt, SingularAttributeImpl stringAtt,
                                         AbstractPluralAttribute pluralAnnotationAtt,
                                         PropertiesSpecification props, Identifier idN)
            throws Exception {
        when(et.getJavaType()).thenReturn(OWLClassN.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassN.class));
        when(et.getName()).thenReturn(OWLClassN.class.getSimpleName());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassN.getClassIri()));
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

        when(props.getJavaField()).thenReturn(OWLClassN.getPropertiesField());
        when(props.getName()).thenReturn(OWLClassN.getPropertiesField().getName());
        when(props.getDeclaringType()).thenReturn(et);
        when(props.getPropertyIdentifierType()).thenReturn(SingularAttribute.class);
        when(props.getPropertyValueType()).thenReturn(String.class);
        when(et.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassOMock(IdentifiableEntityType<OWLClassO> et, SingularAttributeImpl stringAtt,
                                         Identifier idO)
            throws Exception {
        when(et.getJavaType()).thenReturn(OWLClassO.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassO.class));
        when(et.getIdentifier()).thenReturn(idO);
        when(et.getName()).thenReturn(OWLClassO.class.getSimpleName());
        initIdentifier(et, idO, OWLClassO.getUriField(), false);
        when(et.getAttributes()).thenReturn(Collections.singleton(stringAtt));
        when(et.getFieldSpecifications()).thenReturn(Set.of(stringAtt, idO));
        initAttribute(et, stringAtt, new AttributeInfo(OWLClassO.getStringAttributeField(), Attribute.PersistentAttributeType.DATA));
        when(et.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
        when(et.getFieldSpecification(anyString())).thenThrow(IllegalArgumentException.class);
    }

    public static void initOWLClassPMock(IdentifiableEntityType<OWLClassP> et, TypesSpecification types,
                                         PropertiesSpecification props,
                                         SingularAttributeImpl uriAtt, AbstractPluralAttribute urlsAtt,
                                         ListAttribute simpleListAtt, ListAttribute refListAtt, Identifier idP) throws
            Exception {
        when(et.getJavaType()).thenReturn(OWLClassP.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassP.class));
        when(et.getName()).thenReturn(OWLClassP.class.getSimpleName());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassP.getClassIri()));
        initIdentifier(et, idP, OWLClassP.getUriField(), false);
        when(et.getFieldSpecifications()).thenReturn(Set.of(uriAtt, urlsAtt, simpleListAtt, refListAtt, props, types, idP));
        when(et.getAttributes()).thenReturn(Set.of(uriAtt, urlsAtt, simpleListAtt, refListAtt));
        when(et.getFieldSpecification(props.getName())).thenReturn(props);
        when(et.getProperties()).thenReturn(props);
        when(props.getJavaField()).thenReturn(OWLClassP.getPropertiesField());
        when(props.getName()).thenReturn(OWLClassP.getPropertiesField().getName());
        when(props.getDeclaringType()).thenReturn(et);
        when(props.getPropertyIdentifierType()).thenReturn(URI.class);
        when(props.getPropertyValueType()).thenReturn(Object.class);
        when(et.getFieldSpecification(types.getName())).thenReturn(types);
        when(et.getTypes()).thenReturn(types);
        when(types.getJavaField()).thenReturn(OWLClassP.getTypesField());
        when(types.getName()).thenReturn(OWLClassP.getTypesField().getName());
        when(types.getDeclaringType()).thenReturn(et);
        when(types.getJavaType()).thenReturn(Set.class);
        when(types.getElementType()).thenReturn(URI.class);
        initAttribute(et, uriAtt, new AttributeInfo(OWLClassP.getIndividualUriField(), Attribute.PersistentAttributeType.OBJECT));
        initAttribute(et, urlsAtt, new AttributeInfo(OWLClassP.getIndividualUrlsField(), Attribute.PersistentAttributeType.OBJECT).collectionType(CollectionType.SET)
                                                                                                                                  .elementType(URL.class));
        when(simpleListAtt.getName()).thenReturn(OWLClassP.getSimpleListField().getName());
        when(simpleListAtt.getJavaField()).thenReturn(OWLClassP.getSimpleListField());
        when(et.getFieldSpecification(OWLClassP.getSimpleListField().getName())).thenReturn(simpleListAtt);
        when(simpleListAtt.isCollection()).thenReturn(true);
        when(simpleListAtt.isAssociation()).thenReturn(true);
        when(simpleListAtt.getDeclaringType()).thenReturn(et);
        when(simpleListAtt.getCollectionType()).thenReturn(CollectionType.LIST);
        when(simpleListAtt.getBindableJavaType()).thenReturn(URI.class);
        when(simpleListAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(simpleListAtt.getSequenceType()).thenReturn(SequenceType.simple);
        final Field simpleListField = OWLClassP.getSimpleListField();
        when(simpleListAtt.getIRI())
                .thenReturn(IRI.create(simpleListField.getAnnotation(OWLObjectProperty.class).iri()));
        when(simpleListAtt.getListClassIRI())
                .thenReturn(IRI.create(simpleListField.getAnnotation(Sequence.class).listClassIRI()));
        when(simpleListAtt.getHasNextPropertyIRI())
                .thenReturn(IRI.create(simpleListField.getAnnotation(Sequence.class).hasNextPropertyIRI()));

        when(refListAtt.getName()).thenReturn(OWLClassP.getReferencedListField().getName());
        when(refListAtt.getJavaField()).thenReturn(OWLClassP.getReferencedListField());
        when(et.getFieldSpecification(OWLClassP.getReferencedListField().getName())).thenReturn(refListAtt);
        when(refListAtt.isCollection()).thenReturn(true);
        when(refListAtt.isAssociation()).thenReturn(true);
        when(refListAtt.getDeclaringType()).thenReturn(et);
        when(refListAtt.getCollectionType()).thenReturn(CollectionType.LIST);
        when(refListAtt.getBindableJavaType()).thenReturn(URI.class);
        when(refListAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(refListAtt.getSequenceType()).thenReturn(SequenceType.referenced);
        final Field refListField = OWLClassP.getReferencedListField();
        when(refListAtt.getIRI()).thenReturn(IRI.create(refListField.getAnnotation(OWLObjectProperty.class).iri()));
        when(refListAtt.getListClassIRI())
                .thenReturn(IRI.create(refListField.getAnnotation(Sequence.class).listClassIRI()));
        when(refListAtt.getHasNextPropertyIRI())
                .thenReturn(IRI.create(refListField.getAnnotation(Sequence.class).hasNextPropertyIRI()));
        when(refListAtt.getHasContentsPropertyIRI())
                .thenReturn(IRI.create(refListField.getAnnotation(Sequence.class).hasContentsPropertyIRI()));
        when(et.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOwlClassQMock(IdentifiableEntityType<OWLClassQ> et,
                                         MappedSuperclassTypeImpl<QMappedSuperclass> superclassType,
                                         SingularAttributeImpl qStringAtt, SingularAttributeImpl qParentStringAtt,
                                         SingularAttributeImpl qLabelAtt, SingularAttributeImpl qOwlClassAAtt,
                                         Identifier idQ)
            throws Exception {
        when(et.getJavaType()).thenReturn(OWLClassQ.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassQ.class));
        when(et.getName()).thenReturn(OWLClassQ.class.getSimpleName());
        initIdentifier(et, idQ, OWLClassQ.getUriField(), false);
        when(et.getIRI()).thenReturn(IRI.create(OWLClassQ.getClassIri()));
        when(et.getSupertypes()).thenReturn(Collections.singleton(superclassType));
        when(superclassType.getSubtypes()).thenReturn(Collections.singleton(et));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(et.getFieldSpecifications()).thenReturn(Set.of(qStringAtt, qParentStringAtt, qLabelAtt, qOwlClassAAtt, idQ));
        when(et.getAttributes()).thenReturn(Set.of(qStringAtt, qParentStringAtt, qLabelAtt, qOwlClassAAtt));

        initAttribute(et, qStringAtt, new AttributeInfo(OWLClassQ.getStringAttributeField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(et, qParentStringAtt, new AttributeInfo(OWLClassQ.getParentStringField(), Attribute.PersistentAttributeType.DATA));
        initAttribute(et, qLabelAtt, new AttributeInfo(OWLClassQ.getLabelField(), Attribute.PersistentAttributeType.ANNOTATION));
        initAttribute(et, qOwlClassAAtt, new AttributeInfo(OWLClassQ.getOwlClassAField(), Attribute.PersistentAttributeType.OBJECT));
        when(et.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
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
        when(et.getName()).thenReturn(OWLClassS.class.getSimpleName());
        initIdentifier(et, idS, OWLClassS.getUriField(), true);
        when(et.getJavaType()).thenReturn(OWLClassS.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassS.class));
        when(et.getIRI()).thenReturn(IRI.create(OWLClassS.getClassIri()));
        when(et.getFieldSpecifications()).thenReturn(Set.of(sNameAtt, sTypes, idS));
        when(et.getAttributes()).thenReturn(Collections.singleton(sNameAtt));
        when(et.getTypes()).thenReturn(sTypes);
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);

        initAttribute(et, sNameAtt, new AttributeInfo(OWLClassS.getNameField(), Attribute.PersistentAttributeType.ANNOTATION));
        when(sTypes.getJavaField()).thenReturn(OWLClassS.getTypesField());
        when(sTypes.getName()).thenReturn(OWLClassS.getTypesField().getName());
        when(sTypes.getDeclaringType()).thenReturn(et);
        when(sTypes.getJavaType()).thenReturn(Set.class);
        when(sTypes.getElementType()).thenReturn(String.class);
        when(et.getFieldSpecification(sNameAtt.getName())).thenReturn(sNameAtt);
        when(et.getFieldSpecification(sTypes.getName())).thenReturn(sTypes);
        final EntityLifecycleListenerManager listenerManager = new EntityLifecycleListenerManager();
        addLifecycleCallback(listenerManager, PRE_PERSIST, OWLClassS.getPrePersistHook());
        when(et.getLifecycleListenerManager()).thenReturn(listenerManager);
    }

    static void initOwlClassRMock(IdentifiableEntityType<OWLClassR> et, SingularAttributeImpl rStringAtt,
                                  SingularAttributeImpl owlClassAAtt, IdentifiableEntityType<OWLClassS> parentEt)
            throws Exception {
        final Identifier id = parentEt.getIdentifier();
        when(et.getIdentifier()).thenReturn(id);
        when(et.getJavaType()).thenReturn(OWLClassR.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassR.class));
        when(et.getIRI()).thenReturn(IRI.create(OWLClassR.getClassIri()));
        when(et.getName()).thenReturn(OWLClassR.class.getSimpleName());
        final Set attributes = new HashSet<>(parentEt.getAttributes());
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
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
        when(et.getJavaType()).thenReturn(OWLClassT.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassT.class));
        initIdentifier(et, id, OWLClassT.getUriField(), true);
        when(et.getIRI()).thenReturn(IRI.create(OWLClassT.getClassIri()));
        when(et.getName()).thenReturn(OWLClassT.class.getSimpleName());
        when(et.getFieldSpecifications()).thenReturn(Set.of(localDateAtt, localDateTimeAtt, id));
        when(et.getAttributes()).thenReturn(Set.of(localDateAtt, localDateTimeAtt));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);

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
        when(et.getJavaType()).thenReturn(OWLClassU.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassU.class));
        initIdentifier(et, id, OWLClassU.getIdField(), true);
        when(et.getIRI()).thenReturn(IRI.create(OWLClassU.getClassIri()));
        when(et.getName()).thenReturn(OWLClassU.class.getSimpleName());
        when(et.getFieldSpecifications()).thenReturn(Set.of(singularStringAtt, pluralStringAtt, modified, id));
        when(et.getAttributes()).thenReturn(Set.of(singularStringAtt, pluralStringAtt, modified));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);

        initAttribute(et, singularStringAtt, new AttributeInfo(OWLClassU.getSingularStringAttField(), Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(et, modified, new AttributeInfo(OWLClassU.getModifiedField(), Attribute.PersistentAttributeType.DATA).converter(new LocalDateTimeConverter())
                                                                                                                           .language(null));
        initAttribute(et, pluralStringAtt, new AttributeInfo(OWLClassU.getPluralStringAttField(), Attribute.PersistentAttributeType.DATA).collectionType(CollectionType.SET)
                                                                                                                                         .elementType(MultilingualString.class)
                                                                                                                                         .language(null));

        final EntityLifecycleListenerManager listenerManager = new EntityLifecycleListenerManager();
        addLifecycleCallback(listenerManager, PRE_UPDATE, OWLClassU.class.getDeclaredMethod("preUpdate"));
        when(et.getLifecycleListenerManager()).thenReturn(listenerManager);
    }

    static void initOwlClassWMocks(IdentifiableEntityType<OWLClassW> et, AbstractPluralAttribute setStringAtt,
                                   AbstractPluralAttribute listStringAtt, AbstractPluralAttribute collectionStringAtt,
                                   AbstractQueryAttribute setQueryStringAtt, AbstractQueryAttribute listQueryStringAtt,
                                   Identifier id) throws Exception {
        when(et.getJavaType()).thenReturn(OWLClassW.class);
        when(et.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassW.class));
        initIdentifier(et, id, OWLClassW.getIdField(), true);
        when(et.getIRI()).thenReturn(IRI.create(OWLClassW.getClassIri()));
        when(et.getName()).thenReturn(OWLClassW.class.getSimpleName());
        when(et.getFieldSpecifications())
                .thenReturn(Set.of(setStringAtt, listStringAtt, collectionStringAtt, setQueryStringAtt, listQueryStringAtt, id));
        when(et.getAttributes()).thenReturn((Set) Set.of(setStringAtt, listStringAtt, collectionStringAtt, setQueryStringAtt, listQueryStringAtt));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);

        initAttribute(et, setStringAtt,
                new AttributeInfo(OWLClassW.getSetStringAttField(), Attribute.PersistentAttributeType.DATA).collectionType(CollectionType.SET)
                                                                                                           .elementType(MultilingualString.class)
                                                                                                           .language(null));
        initAttribute(et, collectionStringAtt, new AttributeInfo(OWLClassW.getCollectionStringAttField(), Attribute.PersistentAttributeType.DATA).collectionType(CollectionType.COLLECTION)
                                                                                                                                                 .elementType(MultilingualString.class)
                                                                                                                                                 .language(null));

        when(listStringAtt.getJavaField()).thenReturn(OWLClassW.getListStringAttField());
        when(listStringAtt.getJavaType()).thenReturn(OWLClassW.getListStringAttField().getType());
        when(listStringAtt.getName()).thenReturn(OWLClassW.getListStringAttField().getName());
        when(et.getAttribute(OWLClassW.getListStringAttField().getName())).thenReturn(listStringAtt);
        when(et.getFieldSpecification(OWLClassW.getListStringAttField().getName())).thenReturn(listStringAtt);
        when(listStringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(listStringAtt.isCollection()).thenReturn(true);
        when(listStringAtt.getCollectionType()).thenReturn(CollectionType.LIST);
        when(listStringAtt.getBindableJavaType()).thenReturn(MultilingualString.class);
        when(listStringAtt.getIRI()).thenReturn(
                IRI.create(OWLClassW.getListStringAttField().getAnnotation(OWLDataProperty.class).iri()));
        when(listStringAtt.getDeclaringType()).thenReturn(et);
        when(listStringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(listStringAtt.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(listStringAtt.hasLanguage()).thenReturn(false);
        when(listStringAtt.getLanguage()).thenReturn(null);

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
        when(etMock.getJavaType()).thenReturn(OWLClassWithQueryAttr.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(OWLClassWithQueryAttr.class));
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassWithQueryAttr.getClassIri()));
        when(etMock.getAttribute(OWLClassWithQueryAttr.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getQueryAttribute(OWLClassWithQueryAttr.getStrQueryAttField()
                                                           .getName())).thenReturn(strQueryAttMock);
        when(etMock.getName()).thenReturn(OWLClassWithQueryAttr.class.getSimpleName());

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
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initPhoneMocks(IdentifiableEntityType<Phone> etMock, SingularAttributeImpl phoneNumberAttMock,
                                      Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(Phone.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(Phone.class));
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(Vocabulary.c_Phone));
        when(etMock.getName()).thenReturn(Phone.class.getSimpleName());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(phoneNumberAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(phoneNumberAttMock, idMock));
        initAttribute(etMock, phoneNumberAttMock, new AttributeInfo(Phone.class.getDeclaredField("number"), Attribute.PersistentAttributeType.DATA));
        initIdentifier(etMock, idMock, Phone.class.getDeclaredField("uri"), false);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initPersonMocks(IdentifiableEntityType<Person> etMock, SingularAttributeImpl usernameAttMock,
                                       SingularAttributeImpl genderAttMock,
                                       SingularAttributeImpl ageAttMock, SingularAttributeImpl phoneAttMock,
                                       AbstractIdentifiableType<Phone> etPhone,
                                       TypesSpecification typesMock,
                                       Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(Person.class);
        when(etMock.getInstantiableJavaType()).thenReturn((Class) instantiableTypeGenerator.generate(Person.class));
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(Vocabulary.c_Person));
        when(etMock.getName()).thenReturn(Person.class.getSimpleName());
        initIdentifier(etMock, idMock, Person.class.getDeclaredField("uri"), false);
        when(etMock.getAttributes()).thenReturn(Set.of(usernameAttMock, genderAttMock, ageAttMock, phoneAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Set.of(usernameAttMock, genderAttMock, ageAttMock, phoneAttMock, idMock));

        initAttribute(etMock, usernameAttMock, new AttributeInfo(Person.class.getDeclaredField("username"),
                Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(etMock, genderAttMock, new AttributeInfo(Person.class.getDeclaredField("gender"),
                Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(etMock, ageAttMock, new AttributeInfo(Person.class.getDeclaredField("age"),
                Attribute.PersistentAttributeType.DATA).language(null));
        initAttribute(etMock, phoneAttMock, new AttributeInfo(Person.class.getDeclaredField("phone"),
                Attribute.PersistentAttributeType.OBJECT).valueType(etPhone));

        when(typesMock.getJavaField()).thenReturn(Person.class.getDeclaredField("types"));
        when(typesMock.getName()).thenReturn(Person.class.getDeclaredField("types").getName());
        when(typesMock.getDeclaringType()).thenReturn(etMock);
        when(typesMock.getJavaType()).thenReturn(Set.class);
        when(typesMock.getElementType()).thenReturn(String.class);
        when(typesMock.isCollection()).thenReturn(true);
        when(typesMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(etMock.getTypes()).thenReturn(typesMock);
        when(etMock.getFieldSpecification("types")).thenReturn(typesMock);
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
