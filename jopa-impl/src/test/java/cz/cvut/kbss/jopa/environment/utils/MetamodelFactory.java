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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.listener.AnotherListener;
import cz.cvut.kbss.jopa.environment.listener.ConcreteListener;
import cz.cvut.kbss.jopa.environment.listener.ParentListener;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.oom.converter.*;
import cz.cvut.kbss.jopa.oom.converter.datetime.LocalDateTimeConverter;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent.*;
import static org.mockito.Mockito.*;

/**
 * Initializes the specified mock objects to return reasonable values.
 */
@SuppressWarnings({"unchecked", "rawtypes"})
public class MetamodelFactory {

    private MetamodelFactory() {
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassAMocks(EntityTypeImpl<OWLClassA> etMock, AbstractAttribute strAttMock,
                                          TypesSpecification typesMock, Identifier idMock) throws Exception {
        when(etMock.getJavaType()).thenReturn(OWLClassA.class);
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassA.getClassIri()));
        when(etMock.getAttribute(OWLClassA.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getName()).thenReturn(OWLClassA.class.getSimpleName());
        when(etMock.getTypes()).thenReturn(typesMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(
                new HashSet<>(Arrays.<FieldSpecification<? super OWLClassA, ?>>asList(strAttMock, typesMock, idMock)));

        when(strAttMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        when(strAttMock.getJavaType()).thenReturn(OWLClassA.getStrAttField().getType());
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        final String stringAttIri = OWLClassA.getStrAttField().getAnnotation(OWLDataProperty.class).iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getName()).thenReturn(OWLClassA.getStrAttField().getName());
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(strAttMock.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(strAttMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(strAttMock.hasLanguage()).thenReturn(true);
        when(strAttMock.getLanguage()).thenReturn(Generators.LANG);
        when(typesMock.getJavaField()).thenReturn(OWLClassA.getTypesField());
        when(typesMock.getName()).thenReturn(OWLClassA.getTypesField().getName());
        when(typesMock.getDeclaringType()).thenReturn(etMock);
        when(typesMock.getJavaType()).thenReturn(Set.class);
        when(typesMock.getElementType()).thenReturn(String.class);
        when(typesMock.isCollection()).thenReturn(true);
        when(typesMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(etMock.getFieldSpecification(strAttMock.getName())).thenReturn(strAttMock);
        when(etMock.getFieldSpecification(typesMock.getName())).thenReturn(typesMock);

        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification("uri")).thenReturn(idMock);
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

    private static void addLifecycleCallback(EntityLifecycleListenerManager manager, LifecycleEvent evt,
                                             Method callback) throws Exception {
        final Method addCallback = EntityLifecycleListenerManager.class
                .getDeclaredMethod("addLifecycleCallback", LifecycleEvent.class, Method.class);
        if (!addCallback.isAccessible()) {
            addCallback.setAccessible(true);
        }
        addCallback.invoke(manager, evt, callback);
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassBMocks(EntityTypeImpl<OWLClassB> etMock, AbstractAttribute strAttMock,
                                          PropertiesSpecification propsMock, Identifier idMock) throws
            NoSuchFieldException,
            SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassB.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassB.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassB.class.getSimpleName());
        when(etMock.getAttribute(OWLClassB.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getProperties()).thenReturn(propsMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(
                new HashSet<>(Arrays.<FieldSpecification<? super OWLClassB, ?>>asList(strAttMock, propsMock, idMock)));

        when(strAttMock.getJavaField()).thenReturn(OWLClassB.getStrAttField());
        when(strAttMock.getJavaType()).thenReturn(OWLClassB.getStrAttField().getType());
        final String stringAttIri = OWLClassB.getStrAttField().getAnnotation(OWLDataProperty.class).iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getName()).thenReturn(OWLClassB.getStrAttField().getName());
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        when(strAttMock.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(strAttMock.hasLanguage()).thenReturn(true);
        when(strAttMock.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getFieldSpecification(strAttMock.getName())).thenReturn(strAttMock);
        when(propsMock.getJavaField()).thenReturn(OWLClassB.getPropertiesField());
        when(propsMock.getName()).thenReturn(OWLClassB.getPropertiesField().getName());
        when(propsMock.getDeclaringType()).thenReturn(etMock);
        when(propsMock.getPropertyIdentifierType()).thenReturn(String.class);
        when(propsMock.getPropertyValueType()).thenReturn(String.class);

        when(etMock.getFieldSpecification(propsMock.getName())).thenReturn(propsMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassB.class.getDeclaredField("uri"));
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
        when(etMock.getDeclaredAttribute(anyString())).thenAnswer(args -> {
            final String name = args.getArgument(0);
            if (Objects.equals(name, strAttMock.getName())) {
                return strAttMock;
            }
            throw new IllegalArgumentException("Unknown attribute " + name);
        });
    }

    public static void initOWLClassCMocks(EntityTypeImpl<OWLClassC> etMock,
                                          ListAttributeImpl simpleListMock, ListAttributeImpl refListMock,
                                          Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassC.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassC.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassC.class.getSimpleName());
        when(etMock.getAttribute(OWLClassC.getSimpleListField().getName())).thenReturn(
                simpleListMock);
        when(etMock.getAttribute(OWLClassC.getRefListField().getName())).thenReturn(refListMock);
        when(etMock.getAttributes())
                .thenReturn(new HashSet<>(Arrays.<Attribute<? super OWLClassC, ?>>asList(simpleListMock, refListMock)));
        when(etMock.getFieldSpecifications()).thenReturn(
                new HashSet<>(
                        Arrays.<FieldSpecification<? super OWLClassC, ?>>asList(simpleListMock, refListMock, idMock)));
        when(simpleListMock.getJavaField()).thenReturn(OWLClassC.getSimpleListField());
        when(refListMock.getJavaField()).thenReturn(OWLClassC.getRefListField());
        String attIri = OWLClassC.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri();
        when(simpleListMock.getIRI()).thenReturn(IRI.create(attIri));
        when(simpleListMock.getName()).thenReturn(OWLClassC.getSimpleListField().getName());
        when(etMock.getFieldSpecification(simpleListMock.getName())).thenReturn(simpleListMock);
        String hasListAttIri = OWLClassC.getSimpleListField().getAnnotation(Sequence.class)
                                        .ClassOWLListIRI();
        when(simpleListMock.getSequenceType()).thenReturn(SequenceType.simple);
        when(simpleListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(simpleListMock.getOWLListClass()).thenReturn(IRI.create(hasListAttIri));
        String hasNextIri = OWLClassC.getSimpleListField().getAnnotation(Sequence.class)
                                     .ObjectPropertyHasNextIRI();
        when(simpleListMock.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(hasNextIri));
        when(simpleListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(simpleListMock.getPersistentAttributeType())
                .thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(simpleListMock.isCollection()).thenReturn(Boolean.TRUE);
        when(simpleListMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(simpleListMock.getDeclaringType()).thenReturn(etMock);
        when(simpleListMock.getJavaType()).thenReturn(List.class);
        when(simpleListMock.getFetchType()).thenReturn(FetchType.LAZY);
        when(simpleListMock.getCascadeTypes())
                .thenReturn(OWLClassC.getSimpleListField().getAnnotation(OWLObjectProperty.class).cascade());

        hasListAttIri = OWLClassC.getRefListField().getAnnotation(Sequence.class).ClassOWLListIRI();
        when(refListMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(refListMock.getSequenceType()).thenReturn(SequenceType.referenced);
        when(refListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(refListMock.getOWLListClass()).thenReturn(IRI.create(hasListAttIri));
        when(refListMock.getName()).thenReturn(OWLClassC.getRefListField().getName());
        when(etMock.getFieldSpecification(refListMock.getName())).thenReturn(refListMock);
        hasNextIri = OWLClassC.getRefListField().getAnnotation(Sequence.class)
                              .ObjectPropertyHasNextIRI();
        when(refListMock.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(hasNextIri));
        final String contentIri = OWLClassC.getRefListField().getAnnotation(Sequence.class)
                                           .ObjectPropertyHasContentsIRI();
        when(refListMock.getOWLPropertyHasContentsIRI()).thenReturn(IRI.create(contentIri));
        attIri = OWLClassC.getRefListField().getAnnotation(OWLObjectProperty.class).iri();
        when(refListMock.getIRI()).thenReturn(IRI.create(attIri));
        when(refListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(refListMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(refListMock.isCollection()).thenReturn(Boolean.TRUE);
        when(refListMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(refListMock.getDeclaringType()).thenReturn(etMock);
        when(refListMock.getJavaType()).thenReturn(List.class);
        when(refListMock.getCascadeTypes())
                .thenReturn(OWLClassC.getRefListField().getAnnotation(OWLObjectProperty.class).cascade());

        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassC.class.getDeclaredField("uri"));
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(idMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
        when(etMock.getDeclaredAttribute(anyString())).thenAnswer(args -> {
            final String name = args.getArgument(0);
            if (Objects.equals(name, refListMock.getName())) {
                return refListMock;
            } else if (Objects.equals(name, simpleListMock.getName())) {
                return simpleListMock;
            }
            throw new IllegalArgumentException("Unknown attribute " + name);
        });
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassDMocks(EntityTypeImpl<OWLClassD> etMock, SingularAttributeImpl clsAMock,
                                          Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassD.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassD.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassD.class.getSimpleName());
        when(etMock.getAttribute(OWLClassD.getOwlClassAField().getName())).thenReturn(clsAMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.singleton(clsAMock));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(Arrays.<FieldSpecification<? super OWLClassD, ?>>asList(clsAMock, idMock)));
        when(clsAMock.getJavaField()).thenReturn(OWLClassD.getOwlClassAField());
        when(clsAMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final String clsAIri = OWLClassD.getOwlClassAField().getAnnotation(OWLObjectProperty.class)
                                        .iri();
        when(clsAMock.getIRI()).thenReturn(IRI.create(clsAIri));
        when(clsAMock.getJavaType()).thenReturn(OWLClassA.class);
        when(clsAMock.getName()).thenReturn(OWLClassD.getOwlClassAField().getName());
        when(clsAMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(clsAMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(clsAMock.getDeclaringType()).thenReturn(etMock);
        when(clsAMock.isAssociation()).thenReturn(true);
        when(clsAMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(etMock.getFieldSpecification(clsAMock.getName())).thenReturn(clsAMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassD.getUriField());
        when(idMock.getName()).thenReturn(OWLClassD.getUriField().getName());
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(OWLClassD.getUriField().getName())).thenReturn(idMock);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassEMocks(EntityTypeImpl<OWLClassE> etMock, AbstractAttribute strAttMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassE.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassE.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassE.class.getSimpleName());
        when(etMock.getAttribute(OWLClassE.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(strAttMock));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(Arrays.<FieldSpecification<? super OWLClassE, ?>>asList(strAttMock, idMock)));
        when(strAttMock.getJavaField()).thenReturn(OWLClassE.getStrAttField());
        final String stringAttIri = OWLClassE.getStrAttField().getAnnotation(OWLDataProperty.class)
                                             .iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        when(strAttMock.hasLanguage()).thenReturn(true);
        when(strAttMock.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassE.class.getDeclaredField("uri"));
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassFMocks(EntityTypeImpl<OWLClassF> etMock, AbstractPluralAttribute setAMock,
                                          AbstractAttribute strAttMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassF.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassF.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassF.class.getSimpleName());
        when(etMock.getAttribute(OWLClassF.getSimpleSetField().getName())).thenReturn(setAMock);
        when(etMock.getAttributes())
                .thenReturn(new HashSet<>(Arrays.<Attribute<? super OWLClassF, ?>>asList(setAMock, strAttMock)));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(
                        Arrays.<FieldSpecification<? super OWLClassF, ?>>asList(setAMock, strAttMock, idMock)));
        when(setAMock.getJavaField()).thenReturn(OWLClassF.getSimpleSetField());
        when(setAMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(setAMock.getCascadeTypes())
                .thenReturn(OWLClassF.getSimpleSetField().getAnnotation(OWLObjectProperty.class).cascade());
        final String setAIri = OWLClassF.getSimpleSetField().getAnnotation(OWLObjectProperty.class).iri();
        when(setAMock.getIRI()).thenReturn(IRI.create(setAIri));
        when(setAMock.getJavaType()).thenReturn(Set.class);
        when(setAMock.getJavaField()).thenReturn(OWLClassF.getSimpleSetField());
        when(setAMock.isCollection()).thenReturn(Boolean.TRUE);
        when(setAMock.getCollectionType()).thenReturn(CollectionType.SET);
        when(setAMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(setAMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(setAMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getAttribute(OWLClassF.getSimpleSetField().getName())).thenReturn(setAMock);
        when(etMock.getFieldSpecification(OWLClassF.getSimpleSetField().getName())).thenReturn(setAMock);

        when(strAttMock.getJavaField()).thenReturn(OWLClassF.getStrAttField());
        when(strAttMock.getJavaType()).thenReturn(OWLClassF.getStrAttField().getType());
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        final String stringAttIri = OWLClassF.getStrAttField().getAnnotation(OWLDataProperty.class)
                                             .iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getName()).thenReturn(OWLClassF.getStrAttField().getName());
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(strAttMock.isInferred()).thenReturn(true);
        when(strAttMock.hasLanguage()).thenReturn(true);
        when(strAttMock.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getFieldSpecification(OWLClassF.getStrAttField().getName())).thenReturn(strAttMock);

        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassF.class.getDeclaredField("uri"));
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassGMocks(EntityTypeImpl<OWLClassG> etMock, SingularAttributeImpl clsHMock,
                                          EntityTypeImpl<OWLClassH> etHMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassG.class);
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassG.getClassIri()));
        when(etMock.getAttribute(OWLClassG.getOwlClassHField().getName())).thenReturn(clsHMock);
        when(etMock.getName()).thenReturn(OWLClassG.class.getSimpleName());
        when(etMock.getAttributes()).thenReturn(Collections.singleton(clsHMock));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(Arrays.<FieldSpecification<? super OWLClassG, ?>>asList(clsHMock, idMock)));
        when(clsHMock.getJavaField()).thenReturn(OWLClassG.getOwlClassHField());
        when(clsHMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(clsHMock.getIRI()).thenReturn(IRI.create(Vocabulary.p_g_hasH));
        when(clsHMock.getJavaType()).thenReturn(OWLClassH.class);
        when(clsHMock.getName()).thenReturn(OWLClassG.getOwlClassHField().getName());
        when(clsHMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(clsHMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(clsHMock.getDeclaringType()).thenReturn(etMock);
        when(clsHMock.getType()).thenReturn(etHMock);
        when(etMock.getFieldSpecification(clsHMock.getName())).thenReturn(clsHMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassG.class.getDeclaredField("uri"));
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassHMocks(EntityTypeImpl<OWLClassH> etMock, SingularAttributeImpl clsAMock,
                                          SingularAttributeImpl clsGMock, EntityTypeImpl<OWLClassA> etAMock,
                                          EntityTypeImpl<OWLClassG> etGMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassH.class);
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassH.getClassIri()));
        when(etMock.getAttribute(OWLClassH.getOwlClassAField().getName())).thenReturn(clsAMock);
        when(etMock.getName()).thenReturn(OWLClassH.class.getSimpleName());
        when(etMock.getAttributes()).thenReturn(
                new HashSet<>(Arrays.<Attribute<OWLClassH, ?>>asList(clsAMock, clsGMock)));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(
                        Arrays.<FieldSpecification<OWLClassH, ?>>asList(clsAMock, clsGMock, idMock)));
        when(clsAMock.getJavaField()).thenReturn(OWLClassH.getOwlClassAField());
        when(clsAMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(clsAMock.getIRI()).thenReturn(IRI.create(Vocabulary.p_h_hasA));
        when(clsAMock.getJavaType()).thenReturn(OWLClassA.class);
        when(clsAMock.getName()).thenReturn(OWLClassH.getOwlClassAField().getName());
        when(clsAMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(clsAMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(clsAMock.getDeclaringType()).thenReturn(etMock);
        when(clsAMock.getType()).thenReturn(etAMock);

        when(clsGMock.getJavaField()).thenReturn(OWLClassH.getOwlClassGField());
        when(clsGMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(clsGMock.getIRI()).thenReturn(IRI.create(Vocabulary.p_h_hasG));
        when(clsGMock.getJavaType()).thenReturn(OWLClassG.class);
        when(clsGMock.getName()).thenReturn(OWLClassH.getOwlClassGField().getName());
        when(clsGMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(clsGMock.getFetchType()).thenReturn(FetchType.LAZY);
        when(clsGMock.getDeclaringType()).thenReturn(etMock);
        when(clsGMock.getType()).thenReturn(etGMock);

        when(etMock.getFieldSpecification(clsAMock.getName())).thenReturn(clsAMock);
        when(etMock.getFieldSpecification(clsGMock.getName())).thenReturn(clsGMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassH.class.getDeclaredField("uri"));
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassJMocks(EntityTypeImpl<OWLClassJ> etMock, AbstractPluralAttribute setAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassJ.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassJ.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassJ.class.getSimpleName());
        when(etMock.getAttribute(OWLClassJ.getOwlClassAField().getName())).thenReturn(setAMock);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(setAMock));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(Arrays.<FieldSpecification<? super OWLClassJ, ?>>asList(setAMock, idMock)));
        when(setAMock.getJavaField()).thenReturn(OWLClassJ.getOwlClassAField());
        when(setAMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(setAMock.getCascadeTypes())
                .thenReturn(OWLClassJ.getOwlClassAField().getAnnotation(OWLObjectProperty.class).cascade());
        final String clsAIri = OWLClassJ.getOwlClassAField().getAnnotation(OWLObjectProperty.class)
                                        .iri();
        when(setAMock.getIRI()).thenReturn(IRI.create(clsAIri));
        when(setAMock.getJavaType()).thenReturn(Set.class);
        when(setAMock.isCollection()).thenReturn(Boolean.TRUE);
        when(setAMock.getCollectionType()).thenReturn(CollectionType.SET);
        when(setAMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(setAMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(setAMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassJ.class.getDeclaredField("uri"));
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassKMocks(EntityTypeImpl<OWLClassK> etMock, AbstractAttribute clsEMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassK.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassK.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassK.class.getSimpleName());
        when(etMock.getAttribute(OWLClassK.getOwlClassEField().getName())).thenReturn(clsEMock);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(clsEMock));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(Arrays.<FieldSpecification<? super OWLClassK, ?>>asList(clsEMock, idMock)));
        when(clsEMock.getJavaField()).thenReturn(OWLClassK.getOwlClassEField());
        when(clsEMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final String clsEIri = OWLClassK.getOwlClassEField().getAnnotation(OWLObjectProperty.class)
                                        .iri();
        when(clsEMock.getIRI()).thenReturn(IRI.create(clsEIri));
        when(clsEMock.getJavaType()).thenReturn(OWLClassE.class);
        when(clsEMock.getDeclaringType()).thenReturn(etMock);
        when(clsEMock.getFetchType()).thenReturn(FetchType.LAZY);
        when(clsEMock.getName()).thenReturn(OWLClassK.getOwlClassEField().getName());
        when(etMock.getFieldSpecification(clsEMock.getName())).thenReturn(clsEMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassK.class.getDeclaredField("uri"));
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassLMocks(EntityTypeImpl<OWLClassL> etMock, ListAttributeImpl refListMock,
                                          ListAttributeImpl simpleListMock, AbstractPluralAttribute setMock,
                                          AbstractAttribute singleAMock,
                                          Identifier idMock)
            throws NoSuchFieldException {
        when(etMock.getJavaType()).thenReturn(OWLClassL.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassL.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassL.class.getSimpleName());
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassL.class.getDeclaredField("uri"));
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getDeclaredAttributes()).thenReturn(new HashSet<>(
                Arrays.<Attribute<OWLClassL, ?>>asList(refListMock, simpleListMock, setMock, singleAMock)));
        when(etMock.getAttributes()).thenReturn(new HashSet<>(
                Arrays.<Attribute<OWLClassL, ?>>asList(refListMock, simpleListMock, setMock, singleAMock)));
        when(etMock.getFieldSpecifications()).thenReturn(new HashSet<>(
                Arrays.<FieldSpecification<? super OWLClassL, ?>>asList(refListMock, simpleListMock, setMock,
                                                                        simpleListMock, idMock)));

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
        when(refListMock.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(
                OWLClassL.getReferencedListField().getAnnotation(Sequence.class).ObjectPropertyHasNextIRI()));
        when(refListMock.getOWLPropertyHasContentsIRI()).thenReturn(IRI.create(
                OWLClassL.getReferencedListField().getAnnotation(Sequence.class).ObjectPropertyHasContentsIRI()));
        when(etMock.getFieldSpecification(OWLClassL.getReferencedListField().getName())).thenReturn(refListMock);
        when(etMock.getAttribute(OWLClassL.getReferencedListField().getName())).thenReturn(refListMock);
        when(refListMock.getDeclaringType()).thenReturn(etMock);

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
        when(simpleListMock.getOWLObjectPropertyHasNextIRI()).thenReturn(
                IRI.create(OWLClassL.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri()));
        when(etMock.getFieldSpecification(OWLClassL.getSimpleListField().getName())).thenReturn(simpleListMock);
        when(etMock.getAttribute(OWLClassL.getSimpleListField().getName())).thenReturn(simpleListMock);
        when(simpleListMock.getDeclaringType()).thenReturn(etMock);

        when(setMock.getJavaField()).thenReturn(OWLClassL.getSetField());
        when(setMock.getName()).thenReturn(OWLClassL.getSetField().getName());
        when(setMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(setMock.getIRI()).thenReturn(
                IRI.create(OWLClassL.getSetField().getAnnotation(OWLObjectProperty.class).iri()));
        when(setMock.getConstraints()).thenReturn(
                OWLClassL.getSetField().getAnnotation(ParticipationConstraints.class).value());
        when(setMock.getCollectionType()).thenReturn(CollectionType.SET);
        when(setMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(setMock.isCollection()).thenReturn(true);
        when(etMock.getFieldSpecification(OWLClassL.getSetField().getName())).thenReturn(setMock);
        when(etMock.getAttribute(OWLClassL.getSetField().getName())).thenReturn(setMock);
        when(setMock.getDeclaringType()).thenReturn(etMock);

        when(singleAMock.getJavaField()).thenReturn(OWLClassL.getSingleAField());
        when(singleAMock.getName()).thenReturn(OWLClassL.getSingleAField().getName());
        when(singleAMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(singleAMock.getIRI()).thenReturn(
                IRI.create(OWLClassL.getSingleAField().getAnnotation(OWLObjectProperty.class).iri()));
        when(singleAMock.isCollection()).thenReturn(false);
        when(singleAMock.getConstraints()).thenReturn(
                OWLClassL.getSingleAField().getAnnotation(ParticipationConstraints.class).value());
        when(singleAMock.isNonEmpty())
                .thenReturn(OWLClassL.getSingleAField().getAnnotation(ParticipationConstraints.class).nonEmpty());
        when(singleAMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(OWLClassL.getSingleAField().getName())).thenReturn(singleAMock);
        when(etMock.getAttribute(OWLClassL.getSingleAField().getName())).thenReturn(singleAMock);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassMMock(EntityTypeImpl<OWLClassM> etMock, AbstractAttribute booleanAtt,
                                         AbstractAttribute intAtt, SingularAttributeImpl longAtt,
                                         AbstractAttribute doubleAtt, AbstractAttribute dateAtt,
                                         AbstractAttribute enumAtt, AbstractPluralAttribute intSetAtt,
                                         SingularAttributeImpl lexicalFormAtt, SingularAttributeImpl simpleLiteralAtt,
                                         SingularAttributeImpl explicitDatatypeAtt,
                                         SingularAttributeImpl mWithConverterAtt,
                                         SingularAttributeImpl mObjectOneOfEnumAttribute,
                                         Identifier idMock)
            throws Exception {
        when(etMock.getJavaType()).thenReturn(OWLClassM.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassM.getClassIri()));
        when(etMock.getName()).thenReturn(OWLClassM.class.getSimpleName());
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassM.getUriField());
        when(idMock.getName()).thenReturn(OWLClassM.getUriField().getName());
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(idMock.getName())).thenReturn(idMock);
        when(etMock.getAttributes()).thenReturn(
                new HashSet<>(Arrays.<Attribute<? super OWLClassM, ?>>asList(booleanAtt, intAtt, longAtt, doubleAtt,
                                                                             dateAtt, enumAtt, intSetAtt,
                                                                             lexicalFormAtt, simpleLiteralAtt,
                                                                             explicitDatatypeAtt,
                                                                             mObjectOneOfEnumAttribute)));
        when(etMock.getFieldSpecifications()).thenReturn(new HashSet<>(
                Arrays.<FieldSpecification<? super OWLClassM, ?>>asList(booleanAtt, intAtt, longAtt, doubleAtt, dateAtt,
                                                                        enumAtt, intSetAtt, lexicalFormAtt,
                                                                        simpleLiteralAtt, explicitDatatypeAtt,
                                                                        mObjectOneOfEnumAttribute,
                                                                        idMock)));

        when(booleanAtt.getJavaField()).thenReturn(OWLClassM.getBooleanAttributeField());
        when(booleanAtt.getName()).thenReturn(OWLClassM.getBooleanAttributeField().getName());
        when(booleanAtt.getJavaType()).thenReturn(OWLClassM.getBooleanAttributeField().getType());
        when(booleanAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_booleanAttribute));
        when(booleanAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(booleanAtt.isCollection()).thenReturn(false);
        when(booleanAtt.getDeclaringType()).thenReturn(etMock);
        when(booleanAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(booleanAtt.hasLanguage()).thenReturn(true);
        when(booleanAtt.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getFieldSpecification(OWLClassM.getBooleanAttributeField().getName())).thenReturn(booleanAtt);
        when(etMock.getAttribute(OWLClassM.getBooleanAttributeField().getName())).thenReturn(booleanAtt);

        when(intAtt.getJavaField()).thenReturn(OWLClassM.getIntAttributeField());
        when(intAtt.getName()).thenReturn(OWLClassM.getIntAttributeField().getName());
        when(intAtt.getJavaType()).thenReturn(OWLClassM.getIntAttributeField().getType());
        when(intAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_intAttribute));
        when(intAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(intAtt.isCollection()).thenReturn(false);
        when(intAtt.getDeclaringType()).thenReturn(etMock);
        when(intAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(intAtt.getConverter()).thenReturn(new ToIntegerConverter());
        when(intAtt.hasLanguage()).thenReturn(true);
        when(intAtt.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getFieldSpecification(OWLClassM.getIntAttributeField().getName())).thenReturn(intAtt);
        when(etMock.getAttribute(OWLClassM.getIntAttributeField().getName())).thenReturn(intAtt);


        when(longAtt.getJavaField()).thenReturn(OWLClassM.getLongAttributeField());
        when(longAtt.getName()).thenReturn(OWLClassM.getLongAttributeField().getName());
        when(longAtt.getJavaType()).thenReturn(OWLClassM.getLongAttributeField().getType());
        when(longAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_longAttribute));
        when(longAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(longAtt.isCollection()).thenReturn(false);
        when(longAtt.getBindableJavaType()).thenReturn(Long.class);
        when(longAtt.getDeclaringType()).thenReturn(etMock);
        when(longAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(longAtt.getConverter()).thenReturn(new ToLongConverter());
        when(longAtt.hasLanguage()).thenReturn(true);
        when(longAtt.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getFieldSpecification(OWLClassM.getLongAttributeField().getName())).thenReturn(longAtt);
        when(etMock.getAttribute(OWLClassM.getLongAttributeField().getName())).thenReturn(longAtt);

        when(doubleAtt.getJavaField()).thenReturn(OWLClassM.getDoubleAttributeField());
        when(doubleAtt.getName()).thenReturn(OWLClassM.getDoubleAttributeField().getName());
        when(doubleAtt.getJavaType()).thenReturn(OWLClassM.getDoubleAttributeField().getType());
        when(doubleAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_doubleAttribute));
        when(doubleAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(doubleAtt.isCollection()).thenReturn(false);
        when(doubleAtt.getDeclaringType()).thenReturn(etMock);
        when(doubleAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(doubleAtt.getConverter()).thenReturn(new ToDoubleConverter());
        when(doubleAtt.hasLanguage()).thenReturn(true);
        when(doubleAtt.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getFieldSpecification(OWLClassM.getDoubleAttributeField().getName())).thenReturn(doubleAtt);
        when(etMock.getAttribute(OWLClassM.getDoubleAttributeField().getName())).thenReturn(doubleAtt);

        when(dateAtt.getJavaField()).thenReturn(OWLClassM.getDateAttributeField());
        when(dateAtt.getName()).thenReturn(OWLClassM.getDateAttributeField().getName());
        when(dateAtt.getJavaType()).thenReturn(OWLClassM.getDateAttributeField().getType());
        when(dateAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_dateAttribute));
        when(dateAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(dateAtt.isCollection()).thenReturn(false);
        when(dateAtt.getDeclaringType()).thenReturn(etMock);
        when(dateAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(dateAtt.hasLanguage()).thenReturn(true);
        when(dateAtt.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getFieldSpecification(OWLClassM.getDateAttributeField().getName())).thenReturn(dateAtt);
        when(etMock.getAttribute(OWLClassM.getDateAttributeField().getName())).thenReturn(dateAtt);

        when(enumAtt.getJavaField()).thenReturn(OWLClassM.getEnumAttributeField());
        when(enumAtt.getName()).thenReturn(OWLClassM.getEnumAttributeField().getName());
        when(enumAtt.getJavaType()).thenReturn(OWLClassM.getEnumAttributeField().getType());
        when(enumAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_enumAttribute));
        when(enumAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(enumAtt.isCollection()).thenReturn(false);
        when(enumAtt.getDeclaringType()).thenReturn(etMock);
        when(enumAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(enumAtt.getConverter()).thenReturn(new StringEnumConverter<>(OWLClassM.Severity.class));
        when(enumAtt.hasLanguage()).thenReturn(true);
        when(enumAtt.getLanguage()).thenReturn(Generators.LANG);
        when(etMock.getFieldSpecification(OWLClassM.getEnumAttributeField().getName())).thenReturn(enumAtt);
        when(etMock.getAttribute(OWLClassM.getEnumAttributeField().getName())).thenReturn(enumAtt);

        when(intSetAtt.getJavaField()).thenReturn(OWLClassM.getIntegerSetField());
        when(intSetAtt.getName()).thenReturn(OWLClassM.getIntegerSetField().getName());
        when(intSetAtt.getJavaType()).thenReturn(OWLClassM.getIntegerSetField().getType());
        when(intSetAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_IntegerSet));
        when(intSetAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(intSetAtt.isCollection()).thenReturn(true);
        when(intSetAtt.getCollectionType()).thenReturn(CollectionType.SET);
        when(intSetAtt.getDeclaringType()).thenReturn(etMock);
        when(intSetAtt.getConverter()).thenReturn(new ToIntegerConverter());
        final Type typeMock = mock(Type.class);
        when(intSetAtt.getElementType()).thenReturn(typeMock);
        when(intSetAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(intSetAtt.hasLanguage()).thenReturn(true);
        when(intSetAtt.getLanguage()).thenReturn(Generators.LANG);
        when(typeMock.getJavaType()).thenReturn(Integer.class);
        when(etMock.getFieldSpecification(OWLClassM.getIntegerSetField().getName())).thenReturn(intSetAtt);

        when(lexicalFormAtt.getJavaField()).thenReturn(OWLClassM.getLexicalFormField());
        when(lexicalFormAtt.getName()).thenReturn(OWLClassM.getLexicalFormField().getName());
        when(lexicalFormAtt.getJavaType()).thenReturn(OWLClassM.getLexicalFormField().getType());
        when(lexicalFormAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_lexicalForm));
        when(lexicalFormAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(lexicalFormAtt.isCollection()).thenReturn(false);
        when(lexicalFormAtt.getDeclaringType()).thenReturn(etMock);
        when(lexicalFormAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(lexicalFormAtt.getConverter()).thenReturn(new ToLexicalFormConverter());
        when(lexicalFormAtt.isLexicalForm()).thenReturn(true);
        when(etMock.getFieldSpecification(OWLClassM.getLexicalFormField().getName())).thenReturn(lexicalFormAtt);

        when(simpleLiteralAtt.getJavaField()).thenReturn(OWLClassM.getSimpleLiteralField());
        when(simpleLiteralAtt.getName()).thenReturn(OWLClassM.getSimpleLiteralField().getName());
        when(simpleLiteralAtt.getJavaType()).thenReturn(OWLClassM.getSimpleLiteralField().getType());
        when(simpleLiteralAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_simpleLiteral));
        when(simpleLiteralAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(simpleLiteralAtt.isCollection()).thenReturn(false);
        when(simpleLiteralAtt.getDeclaringType()).thenReturn(etMock);
        when(simpleLiteralAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(simpleLiteralAtt.getConverter()).thenReturn(new ToLexicalFormConverter());
        when(simpleLiteralAtt.isSimpleLiteral()).thenReturn(true);
        when(etMock.getFieldSpecification(OWLClassM.getSimpleLiteralField().getName())).thenReturn(simpleLiteralAtt);
        when(etMock.getAttribute(OWLClassM.getSimpleLiteralField().getName())).thenReturn(simpleLiteralAtt);

        when(explicitDatatypeAtt.getJavaField()).thenReturn(OWLClassM.getExplicitDatatypeField());
        when(explicitDatatypeAtt.getName()).thenReturn(OWLClassM.getExplicitDatatypeField().getName());
        when(explicitDatatypeAtt.getJavaType()).thenReturn(OWLClassM.getExplicitDatatypeField().getType());
        when(explicitDatatypeAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_explicitDatatype));
        when(explicitDatatypeAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(explicitDatatypeAtt.isCollection()).thenReturn(false);
        when(explicitDatatypeAtt.getDeclaringType()).thenReturn(etMock);
        when(explicitDatatypeAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(explicitDatatypeAtt.getConverter()).thenReturn(new ToLexicalFormConverter());
        when(explicitDatatypeAtt.isSimpleLiteral()).thenReturn(false);
        when(explicitDatatypeAtt.getDatatype()).thenReturn(OWLClassM.getExplicitDatatypeField()
                                                                    .getAnnotation(OWLDataProperty.class).datatype());
        when(etMock.getFieldSpecification(OWLClassM.getExplicitDatatypeField().getName())).thenReturn(
                explicitDatatypeAtt);
        when(etMock.getAttribute(OWLClassM.getExplicitDatatypeField().getName())).thenReturn(explicitDatatypeAtt);

        when(mWithConverterAtt.getJavaField()).thenReturn(OWLClassM.getWithConverterField());
        when(mWithConverterAtt.getName()).thenReturn(OWLClassM.getWithConverterField().getName());
        when(mWithConverterAtt.getJavaType()).thenReturn(OWLClassM.getWithConverterField().getType());
        when(mWithConverterAtt.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_withConverter));
        when(mWithConverterAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(mWithConverterAtt.isCollection()).thenReturn(false);
        when(mWithConverterAtt.getDeclaringType()).thenReturn(etMock);
        when(mWithConverterAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(mWithConverterAtt.getConverter()).thenReturn(
                new CustomConverterWrapper(new ZoneOffsetConverter(), String.class));
        when(mWithConverterAtt.isSimpleLiteral()).thenReturn(false);
        when(etMock.getFieldSpecification(OWLClassM.getWithConverterField().getName())).thenReturn(mWithConverterAtt);
        when(etMock.getAttribute(OWLClassM.getWithConverterField().getName())).thenReturn(mWithConverterAtt);

        when(mObjectOneOfEnumAttribute.getJavaField()).thenReturn(OWLClassM.getObjectOneOfEnumAttributeField());
        when(mObjectOneOfEnumAttribute.getName()).thenReturn(OWLClassM.getObjectOneOfEnumAttributeField().getName());
        when(mObjectOneOfEnumAttribute.getJavaType()).thenReturn(OneOfEnum.class);
        when(mObjectOneOfEnumAttribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_objectOneOfEnumAttribute));
        when(mObjectOneOfEnumAttribute.getPersistentAttributeType()).thenReturn(
                Attribute.PersistentAttributeType.OBJECT);
        when(mObjectOneOfEnumAttribute.isCollection()).thenReturn(false);
        when(mObjectOneOfEnumAttribute.getDeclaringType()).thenReturn(etMock);
        when(mObjectOneOfEnumAttribute.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(mObjectOneOfEnumAttribute.getConverter()).thenReturn(new ObjectOneOfEnumConverter(OneOfEnum.class));
        when(mObjectOneOfEnumAttribute.hasLanguage()).thenReturn(false);
        when(etMock.getFieldSpecification(OWLClassM.getObjectOneOfEnumAttributeField().getName())).thenReturn(
                mObjectOneOfEnumAttribute);
        when(etMock.getAttribute(OWLClassM.getObjectOneOfEnumAttributeField().getName())).thenReturn(
                mObjectOneOfEnumAttribute);

        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassNMock(EntityTypeImpl<OWLClassN> et, SingularAttributeImpl annotationAtt,
                                         SingularAttributeImpl annotationUriAtt, SingularAttributeImpl stringAtt,
                                         AbstractPluralAttribute pluralAnnotationAtt,
                                         PropertiesSpecification props, Identifier idN)
            throws Exception {
        when(et.getJavaType()).thenReturn(OWLClassN.class);
        when(et.getName()).thenReturn(OWLClassN.class.getSimpleName());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassN.getClassIri()));
        when(et.getIdentifier()).thenReturn(idN);
        when(idN.getJavaField()).thenReturn(OWLClassN.getUriField());
        when(et.getFieldSpecifications()).thenReturn(new HashSet<>(
                Arrays.<FieldSpecification<? super OWLClassN, ?>>asList(annotationAtt, annotationUriAtt, stringAtt,
                                                                        pluralAnnotationAtt, props, idN)));
        when(et.getAttributes()).thenReturn(new HashSet<>(
                Arrays.<Attribute<? super OWLClassN, ?>>asList(annotationAtt, annotationUriAtt, stringAtt,
                                                               pluralAnnotationAtt)));

        when(annotationAtt.getJavaField()).thenReturn(OWLClassN.getAnnotationPropertyField());
        when(annotationAtt.getJavaType()).thenReturn(OWLClassN.getAnnotationPropertyField().getType());
        when(et.getAttribute(OWLClassN.getAnnotationPropertyField().getName())).thenReturn(annotationAtt);
        when(annotationAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(annotationAtt.isCollection()).thenReturn(false);
        when(annotationAtt.getBindableJavaType()).thenReturn(String.class);
        when(annotationAtt.getIRI()).thenReturn(
                IRI.create(OWLClassN.getAnnotationPropertyField().getAnnotation(OWLAnnotationProperty.class).iri()));
        when(annotationAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(annotationAtt.getDeclaringType()).thenReturn(et);
        when(annotationAtt.hasLanguage()).thenReturn(true);
        when(annotationAtt.getLanguage()).thenReturn(Generators.LANG);

        when(annotationUriAtt.getJavaField()).thenReturn(OWLClassN.getAnnotationUriField());
        when(annotationUriAtt.getJavaType()).thenReturn(OWLClassN.getAnnotationUriField().getType());
        when(et.getAttribute(OWLClassN.getAnnotationUriField().getName())).thenReturn(annotationUriAtt);
        when(annotationUriAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(annotationUriAtt.isCollection()).thenReturn(false);
        when(annotationUriAtt.getBindableJavaType()).thenReturn(String.class);
        when(annotationUriAtt.getIRI()).thenReturn(
                IRI.create(OWLClassN.getAnnotationUriField().getAnnotation(OWLAnnotationProperty.class).iri()));
        when(annotationUriAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(annotationUriAtt.getDeclaringType()).thenReturn(et);
        when(annotationUriAtt.hasLanguage()).thenReturn(true);
        when(annotationUriAtt.getLanguage()).thenReturn(Generators.LANG);

        when(stringAtt.getJavaField()).thenReturn(OWLClassN.getStringAttributeField());
        when(stringAtt.getJavaType()).thenReturn(OWLClassN.getStringAttributeField().getType());
        when(et.getAttribute(OWLClassN.getStringAttributeField().getName())).thenReturn(stringAtt);
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(stringAtt.isCollection()).thenReturn(false);
        when(stringAtt.getBindableJavaType()).thenReturn(String.class);
        when(stringAtt.getIRI()).thenReturn(
                IRI.create(OWLClassN.getStringAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(stringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(stringAtt.getDeclaringType()).thenReturn(et);
        when(stringAtt.hasLanguage()).thenReturn(true);
        when(stringAtt.getLanguage()).thenReturn(Generators.LANG);

        when(pluralAnnotationAtt.getJavaField()).thenReturn(OWLClassN.getPluralAnnotationField());
        when(pluralAnnotationAtt.getJavaType()).thenReturn(OWLClassN.getPluralAnnotationField().getType());
        when(et.getAttribute(OWLClassN.getPluralAnnotationField().getName())).thenReturn(pluralAnnotationAtt);
        when(pluralAnnotationAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(pluralAnnotationAtt.isCollection()).thenReturn(true);
        when(pluralAnnotationAtt.getBindableJavaType()).thenReturn(String.class);
        when(pluralAnnotationAtt.getCollectionType()).thenReturn(CollectionType.SET);
        final Type elemType = mock(Type.class);
        when(elemType.getJavaType()).thenReturn(String.class);
        when(pluralAnnotationAtt.getElementType()).thenReturn(elemType);
        when(pluralAnnotationAtt.getIRI()).thenReturn(
                IRI.create(OWLClassN.getPluralAnnotationField().getAnnotation(OWLAnnotationProperty.class).iri()));
        when(pluralAnnotationAtt.getDeclaringType()).thenReturn(et);
        when(pluralAnnotationAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(pluralAnnotationAtt.hasLanguage()).thenReturn(true);
        when(pluralAnnotationAtt.getLanguage()).thenReturn(Generators.LANG);

        when(props.getJavaField()).thenReturn(OWLClassN.getPropertiesField());
        when(props.getName()).thenReturn(OWLClassN.getPropertiesField().getName());
        when(props.getDeclaringType()).thenReturn(et);
        when(props.getPropertyIdentifierType()).thenReturn(SingularAttribute.class);
        when(props.getPropertyValueType()).thenReturn(String.class);
        when(et.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassOMock(EntityTypeImpl<OWLClassO> et, SingularAttributeImpl stringAtt, Identifier idO)
            throws Exception {
        when(et.getIdentifier()).thenReturn(idO);
        when(et.getName()).thenReturn(OWLClassO.class.getSimpleName());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassO.getClassIri()));
        when(idO.getJavaField()).thenReturn(OWLClassO.getUriField());
        when(et.getAttributes()).thenReturn(Collections.singleton(stringAtt));
        when(et.getFieldSpecifications())
                .thenReturn(new HashSet<>(Arrays.<FieldSpecification<? super OWLClassO, ?>>asList(stringAtt, idO)));
        when(et.getFieldSpecification(stringAtt.getName())).thenReturn(stringAtt);
        when(stringAtt.getJavaField()).thenReturn(OWLClassO.getStringAttributeField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(stringAtt.isCollection()).thenReturn(false);
        when(stringAtt.getBindableJavaType()).thenReturn(String.class);
        when(stringAtt.getIRI())
                .thenReturn(IRI.create(OWLClassO.getStringAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(stringAtt.getDeclaringType()).thenReturn(et);
        when(stringAtt.hasLanguage()).thenReturn(true);
        when(stringAtt.getLanguage()).thenReturn(Generators.LANG);
        when(et.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOWLClassPMock(EntityTypeImpl<OWLClassP> et, TypesSpecification types,
                                         PropertiesSpecification props,
                                         SingularAttribute uriAtt, PluralAttribute urlsAtt,
                                         ListAttribute simpleListAtt, ListAttribute refListAtt, Identifier idP) throws
            Exception {
        when(et.getIdentifier()).thenReturn(idP);
        when(et.getName()).thenReturn(OWLClassP.class.getSimpleName());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassP.getClassIri()));
        when(idP.getJavaField()).thenReturn(OWLClassP.getUriField());
        when(et.getFieldSpecifications())
                .thenReturn(
                        new HashSet<>(
                                Arrays.<FieldSpecification<? super OWLClassP, ?>>asList(uriAtt, urlsAtt, simpleListAtt,
                                                                                        refListAtt, props, types,
                                                                                        idP)));
        when(et.getAttributes())
                .thenReturn(new HashSet<>(
                        Arrays.<Attribute<? super OWLClassP, ?>>asList(uriAtt, urlsAtt, simpleListAtt, refListAtt)));
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
        when(et.getFieldSpecification(uriAtt.getName())).thenReturn(uriAtt);
        when(uriAtt.getName()).thenReturn(OWLClassP.getIndividualUriField().getName());
        when(uriAtt.getJavaField()).thenReturn(OWLClassP.getIndividualUriField());
        when(uriAtt.getDeclaringType()).thenReturn(et);
        when(uriAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(uriAtt.isCollection()).thenReturn(false);
        when(uriAtt.getIRI())
                .thenReturn(IRI.create(OWLClassP.getIndividualUriField().getAnnotation(OWLObjectProperty.class).iri()));
        when(uriAtt.getBindableJavaType()).thenReturn(URI.class);
        when(uriAtt.getJavaType()).thenReturn(OWLClassP.getIndividualUriField().getType());
        when(et.getFieldSpecification(urlsAtt.getName())).thenReturn(urlsAtt);
        when(urlsAtt.getName()).thenReturn(OWLClassP.getIndividualUrlsField().getName());
        when(urlsAtt.getJavaField()).thenReturn(OWLClassP.getIndividualUrlsField());
        when(urlsAtt.isCollection()).thenReturn(true);
        when(urlsAtt.getDeclaringType()).thenReturn(et);
        when(urlsAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(urlsAtt.getCollectionType()).thenReturn(CollectionType.SET);
        when(urlsAtt.getBindableJavaType()).thenReturn(URL.class);
        when(urlsAtt.getIRI()).thenReturn(
                IRI.create(OWLClassP.getIndividualUrlsField().getAnnotation(OWLObjectProperty.class).iri()));
        when(simpleListAtt.getName()).thenReturn(OWLClassP.getSimpleListField().getName());
        when(simpleListAtt.getJavaField()).thenReturn(OWLClassP.getSimpleListField());
        when(et.getFieldSpecification(OWLClassP.getSimpleListField().getName())).thenReturn(simpleListAtt);
        when(simpleListAtt.isCollection()).thenReturn(true);
        when(simpleListAtt.getDeclaringType()).thenReturn(et);
        when(simpleListAtt.getCollectionType()).thenReturn(CollectionType.LIST);
        when(simpleListAtt.getBindableJavaType()).thenReturn(URI.class);
        when(simpleListAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(simpleListAtt.getSequenceType()).thenReturn(SequenceType.simple);
        final Field simpleListField = OWLClassP.getSimpleListField();
        when(simpleListAtt.getIRI())
                .thenReturn(IRI.create(simpleListField.getAnnotation(OWLObjectProperty.class).iri()));
        when(simpleListAtt.getOWLListClass())
                .thenReturn(IRI.create(simpleListField.getAnnotation(Sequence.class).ClassOWLListIRI()));
        when(simpleListAtt.getOWLObjectPropertyHasNextIRI())
                .thenReturn(IRI.create(simpleListField.getAnnotation(Sequence.class).ObjectPropertyHasNextIRI()));

        when(refListAtt.getName()).thenReturn(OWLClassP.getReferencedListField().getName());
        when(refListAtt.getJavaField()).thenReturn(OWLClassP.getReferencedListField());
        when(et.getFieldSpecification(OWLClassP.getReferencedListField().getName())).thenReturn(refListAtt);
        when(refListAtt.isCollection()).thenReturn(true);
        when(refListAtt.getDeclaringType()).thenReturn(et);
        when(refListAtt.getCollectionType()).thenReturn(CollectionType.LIST);
        when(refListAtt.getBindableJavaType()).thenReturn(URI.class);
        when(refListAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(refListAtt.getSequenceType()).thenReturn(SequenceType.referenced);
        final Field refListField = OWLClassP.getReferencedListField();
        when(refListAtt.getIRI()).thenReturn(IRI.create(refListField.getAnnotation(OWLObjectProperty.class).iri()));
        when(refListAtt.getOWLListClass())
                .thenReturn(IRI.create(refListField.getAnnotation(Sequence.class).ClassOWLListIRI()));
        when(refListAtt.getOWLObjectPropertyHasNextIRI())
                .thenReturn(IRI.create(refListField.getAnnotation(Sequence.class).ObjectPropertyHasNextIRI()));
        when(refListAtt.getOWLPropertyHasContentsIRI())
                .thenReturn(IRI.create(refListField.getAnnotation(Sequence.class).ObjectPropertyHasContentsIRI()));
        when(et.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initOwlClassQMock(EntityTypeImpl<OWLClassQ> et,
                                         MappedSuperclassTypeImpl<QMappedSuperclass> superclassType,
                                         SingularAttributeImpl qStringAtt, SingularAttributeImpl qParentStringAtt,
                                         SingularAttributeImpl qLabelAtt, SingularAttributeImpl qOwlClassAAtt,
                                         Identifier idQ)
            throws Exception {
        when(et.getIdentifier()).thenReturn(idQ);
        when(et.getJavaType()).thenReturn(OWLClassQ.class);
        when(et.getName()).thenReturn(OWLClassQ.class.getSimpleName());
        when(idQ.getJavaField()).thenReturn(OWLClassQ.getUriField());
        when(idQ.isGenerated()).thenReturn(true);
        when(idQ.getDeclaringType()).thenReturn(superclassType);
        when(et.getIRI()).thenReturn(IRI.create(OWLClassQ.getClassIri()));
        when(et.getSupertype()).thenReturn((AbstractIdentifiableType) superclassType);
        when(superclassType.getSubtypes()).thenReturn(Collections.singleton(et));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(et.getFieldSpecifications())
                .thenReturn(
                        new HashSet<>(
                                Arrays.<FieldSpecification<? super OWLClassQ, ?>>asList(qStringAtt, qParentStringAtt,
                                                                                        qLabelAtt, qOwlClassAAtt,
                                                                                        idQ)));
        when(et.getAttributes())
                .thenReturn(new HashSet<>(
                        Arrays.<Attribute<? super OWLClassQ, ?>>asList(qStringAtt, qParentStringAtt, qLabelAtt,
                                                                       qOwlClassAAtt)));

        when(qStringAtt.getJavaField()).thenReturn(OWLClassQ.getStringAttributeField());
        when(qStringAtt.getJavaType()).thenReturn(OWLClassQ.getStringAttributeField().getType());
        when(qStringAtt.getName()).thenReturn(OWLClassQ.getStringAttributeField().getName());
        when(et.getAttribute(OWLClassQ.getStringAttributeField().getName())).thenReturn(qStringAtt);
        when(qStringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(qStringAtt.isCollection()).thenReturn(false);
        when(qStringAtt.getBindableJavaType()).thenReturn(String.class);
        when(qStringAtt.getIRI()).thenReturn(
                IRI.create(OWLClassQ.getStringAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(qStringAtt.getDeclaringType()).thenReturn(et);
        when(qStringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(qStringAtt.hasLanguage()).thenReturn(true);
        when(qStringAtt.getLanguage()).thenReturn(Generators.LANG);
        when(et.getFieldSpecification(qStringAtt.getName())).thenReturn(qStringAtt);

        when(qParentStringAtt.getJavaField()).thenReturn(OWLClassQ.getParentStringField());
        when(qParentStringAtt.getJavaType()).thenReturn(OWLClassQ.getParentStringField().getType());
        when(qParentStringAtt.getName()).thenReturn(OWLClassQ.getParentStringField().getName());
        when(et.getAttribute(OWLClassQ.getParentStringField().getName())).thenReturn(qParentStringAtt);
        when(qParentStringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(qParentStringAtt.isCollection()).thenReturn(false);
        when(qParentStringAtt.getBindableJavaType()).thenReturn(String.class);
        when(qParentStringAtt.getIRI()).thenReturn(
                IRI.create(OWLClassQ.getParentStringField().getAnnotation(OWLDataProperty.class).iri()));
        when(qParentStringAtt.getDeclaringType()).thenReturn(superclassType);
        when(qParentStringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(qParentStringAtt.hasLanguage()).thenReturn(true);
        when(qParentStringAtt.getLanguage()).thenReturn(Generators.LANG);
        when(et.getFieldSpecification(qParentStringAtt.getName())).thenReturn(qParentStringAtt);

        when(qLabelAtt.getJavaField()).thenReturn(OWLClassQ.getLabelField());
        when(qLabelAtt.getJavaType()).thenReturn(OWLClassQ.getLabelField().getType());
        when(qLabelAtt.getName()).thenReturn(OWLClassQ.getLabelField().getName());
        when(et.getAttribute(OWLClassQ.getLabelField().getName())).thenReturn(qLabelAtt);
        when(qLabelAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(qLabelAtt.isCollection()).thenReturn(false);
        when(qLabelAtt.getBindableJavaType()).thenReturn(String.class);
        when(qLabelAtt.getIRI()).thenReturn(
                IRI.create(OWLClassQ.getLabelField().getAnnotation(OWLAnnotationProperty.class).iri()));
        when(qLabelAtt.getDeclaringType()).thenReturn(superclassType);
        when(qLabelAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(qLabelAtt.hasLanguage()).thenReturn(true);
        when(qLabelAtt.getLanguage()).thenReturn(Generators.LANG);
        when(et.getFieldSpecification(qLabelAtt.getName())).thenReturn(qLabelAtt);

        when(qOwlClassAAtt.getIRI())
                .thenReturn(IRI.create(OWLClassQ.getOwlClassAField().getAnnotation(OWLObjectProperty.class).iri()));
        when(qOwlClassAAtt.getJavaType()).thenReturn(OWLClassA.class);
        when(qOwlClassAAtt.getJavaField()).thenReturn(OWLClassQ.getOwlClassAField());
        when(qOwlClassAAtt.getName()).thenReturn(OWLClassQ.getOwlClassAField().getName());
        when(qOwlClassAAtt.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(qOwlClassAAtt.getFetchType()).thenReturn(FetchType.EAGER);
        when(qOwlClassAAtt.getDeclaringType()).thenReturn(superclassType);
        when(qOwlClassAAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(qOwlClassAAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(et.getFieldSpecification(qOwlClassAAtt.getName())).thenReturn(qOwlClassAAtt);
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
        doReturn(new HashSet<>(Arrays.asList(qLabelAtt, qParentStringAtt, qOwlClassAAtt))).when(superclassType)
                                                                                          .getDeclaredAttributes();
    }

    public static void initOwlClassSMock(EntityTypeImpl<OWLClassS> et, SingularAttributeImpl sNameAtt,
                                         TypesSpecification sTypes, Identifier idS) throws Exception {
        when(et.getName()).thenReturn(OWLClassS.class.getSimpleName());
        when(et.getIdentifier()).thenReturn(idS);
        when(idS.isGenerated()).thenReturn(true);
        when(et.getJavaType()).thenReturn(OWLClassS.class);
        when(idS.getJavaField()).thenReturn(OWLClassS.getUriField());
        when(idS.getDeclaringType()).thenReturn(et);
        when(et.getIRI()).thenReturn(IRI.create(OWLClassS.getClassIri()));
        when(et.getFieldSpecifications()).thenReturn(new HashSet(Arrays.asList(sNameAtt, sTypes, idS)));
        when(et.getAttributes()).thenReturn(Collections.singleton(sNameAtt));
        when(et.getTypes()).thenReturn(sTypes);
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);

        when(sNameAtt.getJavaField()).thenReturn(OWLClassS.getNameField());
        when(sNameAtt.getJavaType()).thenReturn(OWLClassS.getNameField().getType());
        when(sNameAtt.getName()).thenReturn(OWLClassS.getNameField().getName());
        when(et.getAttribute(OWLClassS.getNameField().getName())).thenReturn(sNameAtt);
        when(sNameAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(sNameAtt.isCollection()).thenReturn(false);
        when(sNameAtt.getBindableJavaType()).thenReturn(String.class);
        when(sNameAtt.getIRI()).thenReturn(
                IRI.create(OWLClassS.getNameField().getAnnotation(OWLAnnotationProperty.class).iri()));
        when(sNameAtt.getDeclaringType()).thenReturn(et);
        when(sNameAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(sNameAtt.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(sNameAtt.hasLanguage()).thenReturn(true);
        when(sNameAtt.getLanguage()).thenReturn(Generators.LANG);
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

    static void initOwlClassRMock(EntityTypeImpl<OWLClassR> et, SingularAttributeImpl rStringAtt,
                                  SingularAttributeImpl owlClassAAtt, EntityTypeImpl<OWLClassS> parentEt)
            throws Exception {
        final Identifier id = parentEt.getIdentifier();
        when(et.getIdentifier()).thenReturn(id);
        when(et.getJavaType()).thenReturn(OWLClassR.class);
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
        when(et.getSupertype()).thenReturn((AbstractIdentifiableType) parentEt);
        when(parentEt.getSubtypes()).thenReturn(Collections.singleton(et));
        when(parentEt.hasSubtypes()).thenReturn(true);

        when(rStringAtt.getJavaField()).thenReturn(OWLClassR.getStringAttField());
        when(rStringAtt.getJavaType()).thenReturn(OWLClassR.getStringAttField().getType());
        when(rStringAtt.getName()).thenReturn(OWLClassR.getStringAttField().getName());
        when(et.getAttribute(OWLClassR.getStringAttField().getName())).thenReturn(rStringAtt);
        when(rStringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(rStringAtt.isCollection()).thenReturn(false);
        when(rStringAtt.getBindableJavaType()).thenReturn(String.class);
        when(rStringAtt.getIRI()).thenReturn(
                IRI.create(OWLClassR.getStringAttField().getAnnotation(OWLDataProperty.class).iri()));
        when(rStringAtt.getDeclaringType()).thenReturn(et);
        when(rStringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(rStringAtt.hasLanguage()).thenReturn(true);
        when(rStringAtt.getLanguage()).thenReturn(Generators.LANG);
        when(et.getFieldSpecification(rStringAtt.getName())).thenReturn(rStringAtt);

        when(owlClassAAtt.getIRI())
                .thenReturn(IRI.create(OWLClassR.getOwlClassAField().getAnnotation(OWLObjectProperty.class).iri()));
        when(owlClassAAtt.getJavaType()).thenReturn(OWLClassA.class);
        when(owlClassAAtt.getJavaField()).thenReturn(OWLClassR.getOwlClassAField());
        when(owlClassAAtt.getName()).thenReturn(OWLClassR.getOwlClassAField().getName());
        when(owlClassAAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(owlClassAAtt.getFetchType()).thenReturn(FetchType.EAGER);
        when(owlClassAAtt.getDeclaringType()).thenReturn(et);
        when(owlClassAAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(owlClassAAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(owlClassAAtt.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(et.getFieldSpecification(owlClassAAtt.getName())).thenReturn(owlClassAAtt);
        for (Attribute att : parentEt.getAttributes()) {
            when(et.getAttribute(att.getName())).thenReturn((AbstractAttribute) att);
        }
        for (FieldSpecification fs : parentEt.getFieldSpecifications()) {
            when(et.getFieldSpecification(fs.getName())).thenReturn(fs);
        }
        final EntityLifecycleListenerManager listenerManager = new EntityLifecycleListenerManager();
        final Method setParent = EntityLifecycleListenerManager.class
                .getDeclaredMethod("setParent", EntityLifecycleListenerManager.class);
        setParent.setAccessible(true);
        setParent.invoke(listenerManager, parentEt.getLifecycleListenerManager());
        addLifecycleCallback(listenerManager, PRE_PERSIST, OWLClassR.getPrePersistHook());
        addLifecycleCallback(listenerManager, POST_PERSIST, OWLClassR.getPostPersistHook());
        addLifecycleCallback(listenerManager, PRE_UPDATE, OWLClassR.getPreUpdateHook());
        addLifecycleCallback(listenerManager, POST_UPDATE, OWLClassR.getPostUpdateHook());
        addLifecycleCallback(listenerManager, PRE_REMOVE, OWLClassR.getPreRemoveHook());
        addLifecycleCallback(listenerManager, POST_REMOVE, OWLClassR.getPostRemoveHook());
        addLifecycleCallback(listenerManager, POST_LOAD, OWLClassR.getPostLoadHook());
        when(et.getLifecycleListenerManager()).thenReturn(listenerManager);
    }

    static void initOwlClassSListeners(EntityTypeImpl<OWLClassS> etS, ParentListener listener) throws Exception {
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
        if (!addListenerCallback.isAccessible()) {
            addListenerCallback.setAccessible(true);
        }
        addListenerCallback.invoke(manager, listener, evt, callback);
    }

    static void initOwlClassRListeners(EntityTypeImpl<OWLClassR> etR, EntityTypeImpl<OWLClassS> etS,
                                       ConcreteListener concreteListener, AnotherListener anotherListener)
            throws Exception {
        final EntityLifecycleListenerManager manager = etR.getLifecycleListenerManager();
        final Method setParent = EntityLifecycleListenerManager.class
                .getDeclaredMethod("setParent", EntityLifecycleListenerManager.class);
        setParent.setAccessible(true);
        setParent.invoke(manager, etS.getLifecycleListenerManager());
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

    static void initOwlClassTMock(EntityTypeImpl<OWLClassT> et, SingularAttributeImpl localDateAtt,
                                  SingularAttributeImpl localDateTimeAtt, SingularAttributeImpl owlClassSAtt,
                                  Identifier id) throws Exception {
        when(et.getIdentifier()).thenReturn(id);
        when(id.isGenerated()).thenReturn(true);
        when(et.getJavaType()).thenReturn(OWLClassT.class);
        when(id.getJavaField()).thenReturn(OWLClassT.getUriField());
        when(id.getDeclaringType()).thenReturn(et);
        when(et.getIRI()).thenReturn(IRI.create(OWLClassT.getClassIri()));
        when(et.getName()).thenReturn(OWLClassT.class.getSimpleName());
        when(et.getFieldSpecifications()).thenReturn(new HashSet(Arrays.asList(localDateAtt, localDateTimeAtt, id)));
        when(et.getAttributes()).thenReturn(new HashSet(Arrays.asList(localDateAtt, localDateTimeAtt)));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);

        when(localDateAtt.getJavaField()).thenReturn(OWLClassT.getLocalDateField());
        when(localDateAtt.getJavaType()).thenReturn(OWLClassT.getLocalDateField().getType());
        when(localDateAtt.getName()).thenReturn(OWLClassT.getLocalDateField().getName());
        when(et.getAttribute(OWLClassT.getLocalDateField().getName())).thenReturn(localDateAtt);
        when(localDateAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(localDateAtt.isCollection()).thenReturn(false);
        when(localDateAtt.getBindableJavaType()).thenReturn(LocalDate.class);
        when(localDateAtt.getIRI()).thenReturn(
                IRI.create(OWLClassT.getLocalDateField().getAnnotation(OWLDataProperty.class).iri()));
        when(localDateAtt.getDeclaringType()).thenReturn(et);
        when(localDateAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(localDateAtt.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(localDateAtt.getConverter()).thenReturn(DefaultConverterWrapper.INSTANCE);

        when(localDateTimeAtt.getJavaField()).thenReturn(OWLClassT.getLocalDateTimeField());
        when(localDateTimeAtt.getJavaType()).thenReturn(OWLClassT.getLocalDateTimeField().getType());
        when(localDateTimeAtt.getName()).thenReturn(OWLClassT.getLocalDateTimeField().getName());
        when(et.getAttribute(OWLClassT.getLocalDateTimeField().getName())).thenReturn(localDateTimeAtt);
        when(localDateTimeAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(localDateTimeAtt.isCollection()).thenReturn(false);
        when(localDateTimeAtt.getBindableJavaType()).thenReturn(LocalDateTime.class);
        when(localDateTimeAtt.getIRI()).thenReturn(
                IRI.create(OWLClassT.getLocalDateTimeField().getAnnotation(OWLDataProperty.class).iri()));
        when(localDateTimeAtt.getDeclaringType()).thenReturn(et);
        when(localDateTimeAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(localDateTimeAtt.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(localDateTimeAtt.getConverter()).thenReturn(new LocalDateTimeConverter());

        when(owlClassSAtt.getJavaField()).thenReturn(OWLClassT.getOwlClassSField());
        when(owlClassSAtt.getJavaType()).thenReturn(OWLClassT.getOwlClassSField().getType());
        when(owlClassSAtt.getName()).thenReturn(OWLClassT.getOwlClassSField().getName());
        when(et.getAttribute(OWLClassT.getOwlClassSField().getName())).thenReturn(owlClassSAtt);
        when(owlClassSAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(owlClassSAtt.isCollection()).thenReturn(false);
        when(owlClassSAtt.getBindableJavaType()).thenReturn(OWLClassS.class);
        when(owlClassSAtt.getIRI()).thenReturn(
                IRI.create(OWLClassT.getOwlClassSField().getAnnotation(OWLObjectProperty.class).iri()));
        when(owlClassSAtt.getDeclaringType()).thenReturn(et);
        when(owlClassSAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(owlClassSAtt.getCascadeTypes()).thenReturn(new CascadeType[0]);
    }

    static void initOwlClassUMocks(EntityTypeImpl<OWLClassU> et, SingularAttributeImpl singularStringAtt,
                                   AbstractPluralAttribute pluralStringAtt,
                                   Identifier id) throws Exception {
        when(et.getIdentifier()).thenReturn(id);
        when(id.isGenerated()).thenReturn(true);
        when(et.getJavaType()).thenReturn(OWLClassU.class);
        when(id.getJavaField()).thenReturn(OWLClassU.getIdField());
        when(id.getDeclaringType()).thenReturn(et);
        when(et.getIRI()).thenReturn(IRI.create(OWLClassU.getClassIri()));
        when(et.getName()).thenReturn(OWLClassU.class.getSimpleName());
        when(et.getFieldSpecifications())
                .thenReturn(new HashSet(Arrays.asList(singularStringAtt, pluralStringAtt, id)));
        when(et.getAttributes()).thenReturn(new HashSet(Arrays.asList(singularStringAtt, pluralStringAtt)));
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);

        when(singularStringAtt.getJavaField()).thenReturn(OWLClassU.getSingularStringAttField());
        when(singularStringAtt.getJavaType()).thenReturn(OWLClassU.getSingularStringAttField().getType());
        when(singularStringAtt.getName()).thenReturn(OWLClassU.getSingularStringAttField().getName());
        when(et.getAttribute(OWLClassU.getSingularStringAttField().getName())).thenReturn(singularStringAtt);
        when(singularStringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(singularStringAtt.isCollection()).thenReturn(false);
        when(singularStringAtt.getBindableJavaType()).thenReturn(MultilingualString.class);
        when(singularStringAtt.getIRI()).thenReturn(
                IRI.create(OWLClassU.getSingularStringAttField().getAnnotation(OWLDataProperty.class).iri()));
        when(singularStringAtt.getDeclaringType()).thenReturn(et);
        when(singularStringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(singularStringAtt.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(singularStringAtt.hasLanguage()).thenReturn(false);
        when(singularStringAtt.getLanguage()).thenReturn(null);

        when(pluralStringAtt.getJavaField()).thenReturn(OWLClassU.getPluralStringAttField());
        when(pluralStringAtt.getJavaType()).thenReturn(OWLClassU.getPluralStringAttField().getType());
        when(pluralStringAtt.getName()).thenReturn(OWLClassU.getPluralStringAttField().getName());
        when(et.getAttribute(OWLClassU.getPluralStringAttField().getName())).thenReturn(pluralStringAtt);
        when(pluralStringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(pluralStringAtt.isCollection()).thenReturn(true);
        when(pluralStringAtt.getCollectionType()).thenReturn(CollectionType.SET);
        when(pluralStringAtt.getBindableJavaType()).thenReturn(MultilingualString.class);
        when(pluralStringAtt.getIRI()).thenReturn(
                IRI.create(OWLClassU.getPluralStringAttField().getAnnotation(OWLDataProperty.class).iri()));
        when(pluralStringAtt.getDeclaringType()).thenReturn(et);
        when(pluralStringAtt.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(pluralStringAtt.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(pluralStringAtt.hasLanguage()).thenReturn(false);
        when(pluralStringAtt.getLanguage()).thenReturn(null);
    }

    static void initOWLClassWithQueryAttrMocks(EntityTypeImpl<OWLClassWithQueryAttr> etMock,
                                               AbstractQueryAttribute strQueryAttMock, AbstractAttribute strAttMock,
                                               AbstractQueryAttribute entityQueryAttMock,
                                               AbstractAttribute entityAttMock,
                                               Identifier idMock) throws NoSuchFieldException {
        when(etMock.getJavaType()).thenReturn(OWLClassWithQueryAttr.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassWithQueryAttr.getClassIri()));
        when(etMock.getAttribute(OWLClassWithQueryAttr.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getQueryAttribute(OWLClassWithQueryAttr.getStrQueryAttField()
                                                           .getName())).thenReturn(strQueryAttMock);
        when(etMock.getName()).thenReturn(OWLClassWithQueryAttr.class.getSimpleName());

        when(etMock.getAttributes()).thenReturn(
                new HashSet<>(Arrays.<Attribute<? super OWLClassWithQueryAttr, ?>>asList(strAttMock, entityAttMock)));
        when(etMock.getQueryAttributes()).thenReturn(
                new HashSet<>(Arrays.<QueryAttribute<? super OWLClassWithQueryAttr, ?>>asList(strQueryAttMock,
                                                                                              entityQueryAttMock)));
        when(etMock.getFieldSpecifications()).thenReturn(
                new HashSet<>(Arrays.<FieldSpecification<? super OWLClassWithQueryAttr, ?>>asList(
                        strAttMock, strQueryAttMock, entityAttMock, entityQueryAttMock, idMock)));

        when(strAttMock.getJavaField()).thenReturn(OWLClassWithQueryAttr.getStrAttField());
        when(strAttMock.getJavaType()).thenReturn(OWLClassWithQueryAttr.getStrAttField().getType());
        final String stringAttIri = OWLClassWithQueryAttr.getStrAttField().getAnnotation(OWLDataProperty.class).iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getName()).thenReturn(OWLClassWithQueryAttr.getStrAttField().getName());
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        when(strAttMock.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(strAttMock.hasLanguage()).thenReturn(true);
        when(strAttMock.getLanguage()).thenReturn(Generators.LANG);

        when(entityAttMock.getJavaField()).thenReturn(OWLClassWithQueryAttr.getEntityAttField());
        when(entityAttMock.getJavaType()).thenReturn(OWLClassWithQueryAttr.getEntityAttField().getType());
        final String entityAttIri = OWLClassWithQueryAttr.getEntityAttField().getAnnotation(OWLObjectProperty.class)
                                                         .iri();
        when(entityAttMock.getIRI()).thenReturn(IRI.create(entityAttIri));
        when(entityAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(entityAttMock.getName()).thenReturn(OWLClassWithQueryAttr.getEntityAttField().getName());
        when(entityAttMock.getDeclaringType()).thenReturn(etMock);
        when(entityAttMock.getConstraints()).thenReturn(new ParticipationConstraint[0]);
        when(entityAttMock.hasLanguage()).thenReturn(true);
        when(entityAttMock.getLanguage()).thenReturn(Generators.LANG);

        when(etMock.getFieldSpecification(strAttMock.getName())).thenReturn(strAttMock);
        when(etMock.getFieldSpecification(strQueryAttMock.getName())).thenReturn(strQueryAttMock);
        when(etMock.getFieldSpecification(entityAttMock.getName())).thenReturn(entityAttMock);
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

        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassWithQueryAttr.class.getDeclaredField("uri"));
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initPhoneMocks(EntityTypeImpl<Phone> etMock, AbstractAttribute phoneNumberAttMock,
                                      Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(Phone.class);
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(Vocabulary.c_Phone));
        when(etMock.getName()).thenReturn(Phone.class.getSimpleName());
        when(etMock.getAttribute(Phone.class.getDeclaredField("number").getName())).thenReturn(phoneNumberAttMock);
        when(etMock.getAttributes()).thenReturn(Collections.singleton(phoneNumberAttMock));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(
                        Arrays.<FieldSpecification<? super Phone, ?>>asList(phoneNumberAttMock, idMock)));
        when(phoneNumberAttMock.getJavaField()).thenReturn(Phone.class.getDeclaredField("number"));
        when(phoneNumberAttMock.getIRI()).thenReturn(IRI.create(Vocabulary.p_p_phoneNumber));
        when(phoneNumberAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(phoneNumberAttMock.getDeclaringType()).thenReturn(etMock);
        when(phoneNumberAttMock.hasLanguage()).thenReturn(false);
        when(phoneNumberAttMock.getName()).thenReturn(Phone.class.getDeclaredField("number").getName());
        when(etMock.getAttribute(Phone.class.getDeclaredField("number").getName())).thenReturn(phoneNumberAttMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(Phone.class.getDeclaredField("uri"));
        when(etMock.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
    }

    public static void initPersonMocks(EntityTypeImpl<Person> etMock, AbstractAttribute usernameAttMock,
                                       AbstractAttribute genderAttMock,
                                       AbstractAttribute ageAttMock, SingularAttributeImpl phoneAttMock,
                                       EntityTypeImpl<Phone> etPhone,
                                       Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(Person.class);
        when(etMock.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(etMock.getIRI()).thenReturn(IRI.create(Vocabulary.c_Person));
        when(etMock.getName()).thenReturn(Person.class.getSimpleName());
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(Person.class.getDeclaredField("uri"));
        when(idMock.getName()).thenReturn(Person.class.getDeclaredField("uri").getName());
        when(idMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(idMock.getName())).thenReturn(idMock);
        when(etMock.getAttributes()).thenReturn(
                new HashSet<>(Arrays.<Attribute<? super Person, ?>>asList(usernameAttMock, genderAttMock, ageAttMock,
                                                                          phoneAttMock)));
        when(etMock.getFieldSpecifications()).thenReturn(new HashSet<>(
                Arrays.<FieldSpecification<? super Person, ?>>asList(usernameAttMock, genderAttMock, ageAttMock,
                                                                     phoneAttMock, idMock)));
        when(usernameAttMock.getJavaField()).thenReturn(Person.class.getDeclaredField("username"));
        when(usernameAttMock.getIRI()).thenReturn(IRI.create(Vocabulary.p_p_username));
        when(usernameAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(usernameAttMock.getDeclaringType()).thenReturn(etMock);
        when(usernameAttMock.hasLanguage()).thenReturn(false);
        when(usernameAttMock.getName()).thenReturn(Person.class.getDeclaredField("username").getName());
        when(etMock.getAttribute(Person.class.getDeclaredField("username").getName())).thenReturn(usernameAttMock);

        when(genderAttMock.getJavaField()).thenReturn(Person.class.getDeclaredField("gender"));
        when(genderAttMock.getIRI()).thenReturn(IRI.create(Vocabulary.p_p_gender));
        when(genderAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(genderAttMock.getDeclaringType()).thenReturn(etMock);
        when(genderAttMock.hasLanguage()).thenReturn(false);
        when(genderAttMock.getName()).thenReturn(Person.class.getDeclaredField("gender").getName());
        when(etMock.getAttribute(Person.class.getDeclaredField("gender").getName())).thenReturn(genderAttMock);

        when(ageAttMock.getJavaField()).thenReturn(Person.class.getDeclaredField("age"));
        when(ageAttMock.getIRI()).thenReturn(IRI.create(Vocabulary.p_p_age));
        when(ageAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(ageAttMock.getDeclaringType()).thenReturn(etMock);
        when(ageAttMock.hasLanguage()).thenReturn(false);
        when(ageAttMock.getName()).thenReturn(Person.class.getDeclaredField("age").getName());
        when(etMock.getAttribute(Person.class.getDeclaredField("age").getName())).thenReturn(ageAttMock);

        when(phoneAttMock.getJavaField()).thenReturn(Person.class.getDeclaredField("phone"));
        when(phoneAttMock.getIRI()).thenReturn(IRI.create(Vocabulary.p_p_hasPhone));
        when(phoneAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(phoneAttMock.getDeclaringType()).thenReturn(etMock);
        when(phoneAttMock.hasLanguage()).thenReturn(false);
        when(phoneAttMock.getName()).thenReturn(Person.class.getDeclaredField("phone").getName());
        when(phoneAttMock.getType()).thenReturn(etPhone);
        when(etMock.getAttribute(Person.class.getDeclaredField("phone").getName())).thenReturn(phoneAttMock);
    }
}
