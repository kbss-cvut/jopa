/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.*;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.mockito.Mockito.when;

/**
 * Initializes the specified mock objects to return reasonable values.
 */
public class MetamodelFactory {

    private MetamodelFactory() {
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassAMocks(EntityType<OWLClassA> etMock, Attribute strAttMock,
                                          TypesSpecification typesMock, Identifier idMock) throws NoSuchFieldException,
                                                                                                  SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassA.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassA.getClassIri()));
        when(etMock.getAttribute(OWLClassA.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getTypes()).thenReturn(typesMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassA, ?>>singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(
                new HashSet<>(Arrays.<FieldSpecification<? super OWLClassA, ?>>asList(strAttMock, typesMock)));

        when(strAttMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        when(strAttMock.getJavaType()).thenReturn(OWLClassA.getStrAttField().getType());
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        final String stringAttIri = OWLClassA.getStrAttField().getAnnotation(OWLDataProperty.class).iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getName()).thenReturn(OWLClassA.getStrAttField().getName());
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(strAttMock.getCascadeTypes()).thenReturn(new CascadeType[0]);
        when(typesMock.getJavaField()).thenReturn(OWLClassA.getTypesField());
        when(typesMock.getName()).thenReturn(OWLClassA.getTypesField().getName());
        when(typesMock.getDeclaringType()).thenReturn(etMock);
        when(typesMock.getJavaType()).thenReturn(String.class);
        when(etMock.getFieldSpecification(strAttMock.getName())).thenReturn(strAttMock);
        when(etMock.getFieldSpecification(typesMock.getName())).thenReturn(typesMock);

        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassBMocks(EntityType<OWLClassB> etMock, Attribute strAttMock,
                                          PropertiesSpecification propsMock, Identifier idMock) throws
                                                                                                NoSuchFieldException,
                                                                                                SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassB.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassB.getClassIri()));
        when(etMock.getAttribute(OWLClassB.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getProperties()).thenReturn(propsMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassB, ?>>singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(
                new HashSet<>(Arrays.<FieldSpecification<? super OWLClassB, ?>>asList(strAttMock, propsMock)));

        when(strAttMock.getJavaField()).thenReturn(OWLClassB.getStrAttField());
        when(strAttMock.getJavaType()).thenReturn(OWLClassB.getStrAttField().getType());
        final String stringAttIri = OWLClassB.getStrAttField().getAnnotation(OWLDataProperty.class).iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getName()).thenReturn(OWLClassB.getStrAttField().getName());
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(strAttMock.getName())).thenReturn(strAttMock);
        when(propsMock.getJavaField()).thenReturn(OWLClassB.getPropertiesField());
        when(propsMock.getName()).thenReturn(OWLClassB.getPropertiesField().getName());
        when(propsMock.getDeclaringType()).thenReturn(etMock);
        when(propsMock.getPropertyIdentifierType()).thenReturn(String.class);
        when(propsMock.getPropertyValueType()).thenReturn(String.class);
        when(etMock.getFieldSpecification(propsMock.getName())).thenReturn(propsMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassB.class.getDeclaredField("uri"));
    }

    public static void initOWLClassCMocks(EntityType<OWLClassC> etMock,
                                          ListAttribute simpleListMock, ListAttribute refListMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassC.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassC.getClassIri()));
        when(etMock.getAttribute(OWLClassC.getSimpleListField().getName())).thenReturn(
                simpleListMock);
        when(etMock.getAttribute(OWLClassC.getRefListField().getName())).thenReturn(refListMock);
        when(etMock.getAttributes())
                .thenReturn(new HashSet<>(Arrays.<Attribute<? super OWLClassC, ?>>asList(simpleListMock, refListMock)));
        when(etMock.getFieldSpecifications()).thenReturn(
                new HashSet<>(Arrays.<FieldSpecification<? super OWLClassC, ?>>asList(simpleListMock, refListMock)));
        when(simpleListMock.getJavaField()).thenReturn(OWLClassC.getSimpleListField());
        when(refListMock.getJavaField()).thenReturn(OWLClassC.getRefListField());
        String attIri = OWLClassC.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri();
        when(simpleListMock.getIRI()).thenReturn(IRI.create(attIri));
        when(simpleListMock.getName()).thenReturn(OWLClassC.getSimpleListField().getName());
        when(etMock.getFieldSpecification(simpleListMock.getName())).thenReturn(simpleListMock);
        String hasListAttIri = OWLClassC.getSimpleListField().getAnnotation(Sequence.class)
                                        .ClassOWLListIRI();
        when(simpleListMock.getSequenceType()).thenReturn(SequenceType.simple);
        when(simpleListMock.getCollectionType()).thenReturn(PluralAttribute.CollectionType.LIST);
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
        when(simpleListMock.getCascadeTypes())
                .thenReturn(OWLClassC.getSimpleListField().getAnnotation(OWLObjectProperty.class).cascade());

        hasListAttIri = OWLClassC.getRefListField().getAnnotation(Sequence.class).ClassOWLListIRI();
        when(refListMock.getSequenceType()).thenReturn(SequenceType.referenced);
        when(refListMock.getCollectionType()).thenReturn(PluralAttribute.CollectionType.LIST);
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
        when(refListMock.getCascadeTypes())
                .thenReturn(OWLClassC.getRefListField().getAnnotation(OWLObjectProperty.class).cascade());

        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassC.class.getDeclaredField("uri"));
    }

    public static void initOWLClassEMocks(EntityType<OWLClassE> etMock, Attribute strAttMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassE.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassE.getClassIri()));
        when(etMock.getAttribute(OWLClassE.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassE, ?>>singleton(strAttMock));
        when(etMock.getFieldSpecifications()).thenReturn(Collections.singleton(strAttMock));
        when(strAttMock.getJavaField()).thenReturn(OWLClassB.getStrAttField());
        final String stringAttIri = OWLClassB.getStrAttField().getAnnotation(OWLDataProperty.class)
                                             .iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassE.class.getDeclaredField("uri"));
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassDMocks(EntityType<OWLClassD> etMock, Attribute clsAMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassD.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassD.getClassIri()));
        when(etMock.getAttribute(OWLClassD.getOwlClassAField().getName())).thenReturn(clsAMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassD, ?>>singleton(clsAMock));
        when(etMock.getFieldSpecifications()).thenReturn(Collections.singleton(clsAMock));
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
        when(etMock.getFieldSpecification(clsAMock.getName())).thenReturn(clsAMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassD.class.getDeclaredField("uri"));
    }

    public static void initOWLClassFMocks(EntityType<OWLClassF> etMock, PluralAttribute setAMock,
                                          SingularAttribute strAttMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassF.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassF.getClassIri()));
        when(etMock.getAttribute(OWLClassF.getSimpleSetField().getName())).thenReturn(setAMock);
        when(etMock.getAttributes())
                .thenReturn(new HashSet<>(Arrays.<Attribute<? super OWLClassF, ?>>asList(setAMock, strAttMock)));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(Arrays.<Attribute<? super OWLClassF, ?>>asList(setAMock, strAttMock)));
        when(setAMock.getJavaField()).thenReturn(OWLClassF.getSimpleSetField());
        when(setAMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(setAMock.getCascadeTypes())
                .thenReturn(OWLClassF.getSimpleSetField().getAnnotation(OWLObjectProperty.class).cascade());
        final String setAIri = OWLClassF.getSimpleSetField().getAnnotation(OWLObjectProperty.class).iri();
        when(setAMock.getIRI()).thenReturn(IRI.create(setAIri));
        when(setAMock.getJavaType()).thenReturn(Set.class);
        when(setAMock.isCollection()).thenReturn(Boolean.TRUE);
        when(setAMock.getCollectionType()).thenReturn(PluralAttribute.CollectionType.SET);
        when(setAMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(setAMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(setAMock.getDeclaringType()).thenReturn(etMock);

        when(strAttMock.getJavaField()).thenReturn(OWLClassF.getStrAttField());
        when(strAttMock.getDeclaringType()).thenReturn(etMock);
        final String stringAttIri = OWLClassF.getStrAttField().getAnnotation(OWLDataProperty.class)
                                             .iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getName()).thenReturn(OWLClassF.getStrAttField().getName());
        when(strAttMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(strAttMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});

        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassF.class.getDeclaredField("uri"));
    }

    public static void iniOWLClassGMocks(EntityType<OWLClassG> etMock, Attribute clsHMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassG.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassG.getClassIri()));
        when(etMock.getAttribute(OWLClassG.getOwlClassHField().getName())).thenReturn(clsHMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassG, ?>>singleton(clsHMock));
        when(etMock.getFieldSpecifications()).thenReturn(Collections.singleton(clsHMock));
        when(clsHMock.getJavaField()).thenReturn(OWLClassG.getOwlClassHField());
        when(clsHMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final String clsHIri = OWLClassD.getOwlClassAField().getAnnotation(OWLObjectProperty.class)
                                        .iri();
        when(clsHMock.getIRI()).thenReturn(IRI.create(clsHIri));
        when(clsHMock.getJavaType()).thenReturn(OWLClassH.class);
        when(clsHMock.getName()).thenReturn(OWLClassG.getOwlClassHField().getName());
        when(clsHMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(clsHMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(clsHMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(clsHMock.getName())).thenReturn(clsHMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassG.class.getDeclaredField("uri"));
    }

    public static void initOWLClassHMocks(EntityType<OWLClassH> etMock, Attribute clsAMock, Attribute clsGMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassH.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassH.getClassIri()));
        when(etMock.getAttribute(OWLClassH.getOwlClassAField().getName())).thenReturn(clsAMock);
        when(etMock.getAttributes())
                .thenReturn(new HashSet<>(Arrays.<Attribute<? super OWLClassH, ?>>asList(clsAMock, clsGMock)));
        when(etMock.getFieldSpecifications())
                .thenReturn(new HashSet<>(Arrays.<FieldSpecification<? super OWLClassH, ?>>asList(clsAMock, clsGMock)));
        when(clsAMock.getJavaField()).thenReturn(OWLClassH.getOwlClassAField());
        when(clsAMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final String clsAIri = OWLClassH.getOwlClassAField().getAnnotation(OWLObjectProperty.class)
                                        .iri();
        when(clsAMock.getIRI()).thenReturn(IRI.create(clsAIri));
        when(clsAMock.getJavaType()).thenReturn(OWLClassA.class);
        when(clsAMock.getName()).thenReturn(OWLClassH.getOwlClassAField().getName());
        when(clsAMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(clsAMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(clsAMock.getDeclaringType()).thenReturn(etMock);

        when(clsGMock.getJavaField()).thenReturn(OWLClassH.getOwlClassGField());
        when(clsGMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final String clsGIri = OWLClassH.getOwlClassGField().getAnnotation(OWLObjectProperty.class)
                                        .iri();
        when(clsGMock.getIRI()).thenReturn(IRI.create(clsGIri));
        when(clsGMock.getJavaType()).thenReturn(OWLClassG.class);
        when(clsGMock.getName()).thenReturn(OWLClassH.getOwlClassGField().getName());
        when(clsGMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(clsGMock.getFetchType()).thenReturn(FetchType.LAZY);
        when(clsGMock.getDeclaringType()).thenReturn(etMock);

        when(etMock.getFieldSpecification(clsAMock.getName())).thenReturn(clsAMock);
        when(etMock.getFieldSpecification(clsGMock.getName())).thenReturn(clsGMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassH.class.getDeclaredField("uri"));
    }

    public static void initOWLClassJMocks(EntityType<OWLClassJ> etMock, PluralAttribute setAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassJ.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassJ.getClassIri()));
        when(etMock.getAttribute(OWLClassJ.getOwlClassAField().getName())).thenReturn(setAMock);
        when(etMock.getAttributes()).thenReturn(Collections.<Attribute<? super OWLClassJ, ?>>singleton(setAMock));
        when(etMock.getFieldSpecifications()).thenReturn(Collections.singleton(setAMock));
        when(setAMock.getJavaField()).thenReturn(OWLClassJ.getOwlClassAField());
        when(setAMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(setAMock.getCascadeTypes())
                .thenReturn(OWLClassJ.getOwlClassAField().getAnnotation(OWLObjectProperty.class).cascade());
        final String clsAIri = OWLClassJ.getOwlClassAField().getAnnotation(OWLObjectProperty.class)
                                        .iri();
        when(setAMock.getIRI()).thenReturn(IRI.create(clsAIri));
        when(setAMock.getJavaType()).thenReturn(Set.class);
        when(setAMock.isCollection()).thenReturn(Boolean.TRUE);
        when(setAMock.getCollectionType()).thenReturn(PluralAttribute.CollectionType.SET);
        when(setAMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(setAMock.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(setAMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassJ.class.getDeclaredField("uri"));
    }

    public static void initOWLClassKMocks(EntityType<OWLClassK> etMock, Attribute clsEMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassK.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassK.getClassIri()));
        when(etMock.getAttribute(OWLClassK.getOwlClassEField().getName())).thenReturn(clsEMock);
        when(etMock.getAttributes()).thenReturn(Collections.<Attribute<? super OWLClassK, ?>>singleton(clsEMock));
        when(etMock.getFieldSpecifications()).thenReturn(Collections.singleton(clsEMock));
        when(clsEMock.getJavaField()).thenReturn(OWLClassK.getOwlClassEField());
        when(clsEMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final String clsEIri = OWLClassK.getOwlClassEField().getAnnotation(OWLObjectProperty.class)
                                        .iri();
        when(clsEMock.getIRI()).thenReturn(IRI.create(clsEIri));
        when(clsEMock.getJavaType()).thenReturn(OWLClassE.class);
        when(clsEMock.getDeclaringType()).thenReturn(etMock);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassK.class.getDeclaredField("uri"));
    }

    public static void initOWLClassLMocks(EntityType<OWLClassL> etMock, ListAttribute refListMock,
                                          ListAttribute simpleListMock, PluralAttribute setMock, Attribute singleAMock,
                                          Identifier idMock)
            throws NoSuchFieldException {
        when(etMock.getJavaType()).thenReturn(OWLClassL.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassL.getClassIri()));
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassL.class.getDeclaredField("uri"));
        when(etMock.getDeclaredAttributes()).thenReturn(new HashSet<>(
                Arrays.<Attribute<OWLClassL, ?>>asList(refListMock, simpleListMock, setMock, singleAMock)));
        when(etMock.getAttributes()).thenReturn(new HashSet<>(
                Arrays.<Attribute<OWLClassL, ?>>asList(refListMock, simpleListMock, setMock, singleAMock)));
        when(etMock.getFieldSpecifications()).thenReturn(new HashSet<>(
                Arrays.<FieldSpecification<? super OWLClassL, ?>>asList(refListMock, simpleListMock, setMock,
                        simpleListMock)));

        when(refListMock.getJavaField()).thenReturn(OWLClassL.getReferencedListField());
        when(refListMock.getName()).thenReturn(OWLClassL.getReferencedListField().getName());
        when(refListMock.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(refListMock.getIRI()).thenReturn(
                IRI.create(OWLClassL.getReferencedListField().getAnnotation(OWLObjectProperty.class).iri()));
        when(refListMock.getConstraints()).thenReturn(
                OWLClassL.getReferencedListField().getAnnotation(ParticipationConstraints.class).value());
        when(refListMock.getCollectionType()).thenReturn(PluralAttribute.CollectionType.LIST);
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
        when(simpleListMock.getCollectionType()).thenReturn(PluralAttribute.CollectionType.LIST);
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
        when(setMock.getCollectionType()).thenReturn(PluralAttribute.CollectionType.SET);
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
    }

    public static void initOWLClassMMock(EntityType<OWLClassM> etMock, SingularAttribute booleanAtt,
                                         SingularAttribute intAtt, SingularAttribute longAtt,
                                         SingularAttribute doubleAtt, SingularAttribute dateAtt,
                                         SingularAttribute enumAtt, Identifier idMock) throws Exception {
        when(etMock.getJavaType()).thenReturn(OWLClassM.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassM.getClassIri()));
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassM.getUriField());
        when(etMock.getAttributes()).thenReturn(
                new HashSet<>(Arrays.<Attribute<? super OWLClassM, ?>>asList(booleanAtt, intAtt, longAtt, doubleAtt,
                        dateAtt, enumAtt)));
        when(etMock.getFieldSpecifications()).thenReturn(new HashSet<>(
                Arrays.<FieldSpecification<? super OWLClassM, ?>>asList(booleanAtt, intAtt, longAtt, doubleAtt, dateAtt,
                        enumAtt)));

        when(booleanAtt.getJavaField()).thenReturn(OWLClassM.getBooleanAttributeField());
        when(booleanAtt.getJavaType()).thenReturn(OWLClassM.getBooleanAttributeField().getType());
        when(booleanAtt.getIRI()).thenReturn(
                IRI.create(OWLClassM.getBooleanAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(booleanAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(booleanAtt.isCollection()).thenReturn(false);
        when(booleanAtt.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(OWLClassM.getBooleanAttributeField().getName())).thenReturn(booleanAtt);

        when(intAtt.getJavaField()).thenReturn(OWLClassM.getIntAttributeField());
        when(intAtt.getJavaType()).thenReturn(OWLClassM.getIntAttributeField().getType());
        when(intAtt.getIRI()).thenReturn(
                IRI.create(OWLClassM.getIntAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(intAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(intAtt.isCollection()).thenReturn(false);
        when(intAtt.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(OWLClassM.getIntAttributeField().getName())).thenReturn(intAtt);

        when(longAtt.getJavaField()).thenReturn(OWLClassM.getLongAttributeField());
        when(longAtt.getJavaType()).thenReturn(OWLClassM.getLongAttributeField().getType());
        when(longAtt.getIRI()).thenReturn(
                IRI.create(OWLClassM.getLongAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(longAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(longAtt.isCollection()).thenReturn(false);
        when(longAtt.getBindableJavaType()).thenReturn(Long.class);
        when(longAtt.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(OWLClassM.getLongAttributeField().getName())).thenReturn(longAtt);

        when(doubleAtt.getJavaField()).thenReturn(OWLClassM.getDoubleAttributeField());
        when(doubleAtt.getJavaType()).thenReturn(OWLClassM.getDoubleAttributeField().getType());
        when(doubleAtt.getIRI()).thenReturn(
                IRI.create(OWLClassM.getDoubleAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(doubleAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(doubleAtt.isCollection()).thenReturn(false);
        when(doubleAtt.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(OWLClassM.getDoubleAttributeField().getName())).thenReturn(doubleAtt);

        when(dateAtt.getJavaField()).thenReturn(OWLClassM.getDateAttributeField());
        when(dateAtt.getJavaType()).thenReturn(OWLClassM.getDateAttributeField().getType());
        when(dateAtt.getIRI())
                .thenReturn(IRI.create(OWLClassM.getDateAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(dateAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(dateAtt.isCollection()).thenReturn(false);
        when(dateAtt.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(OWLClassM.getDateAttributeField().getName())).thenReturn(dateAtt);

        when(enumAtt.getJavaField()).thenReturn(OWLClassM.getEnumAttributeField());
        when(enumAtt.getJavaType()).thenReturn(OWLClassM.getEnumAttributeField().getType());
        when(enumAtt.getIRI()).thenReturn(IRI.create(
                OWLClassM.getEnumAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(enumAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(enumAtt.isCollection()).thenReturn(false);
        when(enumAtt.getDeclaringType()).thenReturn(etMock);
        when(etMock.getFieldSpecification(OWLClassM.getEnumAttributeField().getName())).thenReturn(enumAtt);
    }

    public static void initOWLClassNMock(EntityType<OWLClassN> et, SingularAttribute annotationAtt,
                                         SingularAttribute annotationUriAtt, SingularAttribute stringAtt,
                                         PropertiesSpecification props, Identifier idN)
            throws Exception {
        when(et.getIdentifier()).thenReturn(idN);
        when(idN.getJavaField()).thenReturn(OWLClassN.getUriField());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassN.getClassIri()));
        when(et.getFieldSpecifications()).thenReturn(new HashSet<>(
                Arrays.<FieldSpecification<? super OWLClassN, ?>>asList(annotationAtt, annotationUriAtt, stringAtt,
                        props)));
        when(et.getAttributes()).thenReturn(new HashSet<>(
                Arrays.<Attribute<? super OWLClassN, ?>>asList(annotationAtt, annotationUriAtt, stringAtt)));

        when(annotationAtt.getJavaField()).thenReturn(OWLClassN.getAnnotationPropertyField());
        when(annotationAtt.getJavaType()).thenReturn(OWLClassN.getAnnotationPropertyField().getType());
        when(et.getAttribute(OWLClassN.getAnnotationPropertyField().getName())).thenReturn(annotationAtt);
        when(annotationAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(annotationAtt.isCollection()).thenReturn(false);
        when(annotationAtt.getBindableJavaType()).thenReturn(String.class);
        when(annotationAtt.getIRI()).thenReturn(
                IRI.create(OWLClassN.getAnnotationPropertyField().getAnnotation(OWLAnnotationProperty.class).iri()));
        when(annotationAtt.getDeclaringType()).thenReturn(et);

        when(annotationUriAtt.getJavaField()).thenReturn(OWLClassN.getAnnotationUriField());
        when(annotationUriAtt.getJavaType()).thenReturn(OWLClassN.getAnnotationUriField().getType());
        when(et.getAttribute(OWLClassN.getAnnotationUriField().getName())).thenReturn(annotationUriAtt);
        when(annotationUriAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(annotationUriAtt.isCollection()).thenReturn(false);
        when(annotationUriAtt.getBindableJavaType()).thenReturn(String.class);
        when(annotationUriAtt.getIRI()).thenReturn(
                IRI.create(OWLClassN.getAnnotationUriField().getAnnotation(OWLAnnotationProperty.class).iri()));
        when(annotationUriAtt.getDeclaringType()).thenReturn(et);

        when(stringAtt.getJavaField()).thenReturn(OWLClassN.getStringAttributeField());
        when(stringAtt.getJavaType()).thenReturn(OWLClassN.getStringAttributeField().getType());
        when(et.getAttribute(OWLClassN.getAnnotationPropertyField().getName())).thenReturn(stringAtt);
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(stringAtt.isCollection()).thenReturn(false);
        when(stringAtt.getBindableJavaType()).thenReturn(String.class);
        when(stringAtt.getIRI()).thenReturn(
                IRI.create(OWLClassN.getStringAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(stringAtt.getDeclaringType()).thenReturn(et);

        when(props.getJavaField()).thenReturn(OWLClassN.getPropertiesField());
        when(props.getName()).thenReturn(OWLClassN.getPropertiesField().getName());
        when(props.getDeclaringType()).thenReturn(et);
        when(props.getPropertyIdentifierType()).thenReturn(SingularAttribute.class);
        when(props.getPropertyValueType()).thenReturn(String.class);
    }

    public static void initOWLClassOMock(EntityType<OWLClassO> et, SingularAttribute stringAtt, Identifier idO)
            throws Exception {
        when(et.getIdentifier()).thenReturn(idO);
        when(idO.getJavaField()).thenReturn(OWLClassO.getUriField());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassO.getClassIri()));
        when(et.getAttributes()).thenReturn(Collections.singleton(stringAtt));
        when(et.getFieldSpecifications()).thenReturn(Collections.singleton(stringAtt));
        when(et.getFieldSpecification(stringAtt.getName())).thenReturn(stringAtt);
        when(stringAtt.getJavaField()).thenReturn(OWLClassO.getStringAttributeField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(stringAtt.isCollection()).thenReturn(false);
        when(stringAtt.getBindableJavaType()).thenReturn(String.class);
        when(stringAtt.getIRI())
                .thenReturn(IRI.create(OWLClassO.getStringAttributeField().getAnnotation(OWLDataProperty.class).iri()));
        when(stringAtt.getDeclaringType()).thenReturn(et);
    }

    public static void initOWLClassPMock(EntityType<OWLClassP> et, TypesSpecification types,
                                         PropertiesSpecification props,
                                         SingularAttribute uriAtt, PluralAttribute urlsAtt,
                                         ListAttribute simpleListAtt, ListAttribute refListAtt, Identifier idP) throws
                                                                                                                Exception {
        when(et.getIdentifier()).thenReturn(idP);
        when(idP.getJavaField()).thenReturn(OWLClassP.getUriField());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassP.getClassIri()));
        when(et.getFieldSpecifications())
                .thenReturn(
                        new HashSet<>(
                                Arrays.<FieldSpecification<? super OWLClassP, ?>>asList(uriAtt, urlsAtt, simpleListAtt,
                                        refListAtt, props, types)));
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
        when(types.getJavaType()).thenReturn(URI.class);
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
        when(urlsAtt.getCollectionType()).thenReturn(PluralAttribute.CollectionType.SET);
        when(urlsAtt.getBindableJavaType()).thenReturn(URL.class);
        when(urlsAtt.getIRI()).thenReturn(
                IRI.create(OWLClassP.getIndividualUrlsField().getAnnotation(OWLObjectProperty.class).iri()));
        when(simpleListAtt.getName()).thenReturn(OWLClassP.getSimpleListField().getName());
        when(simpleListAtt.getJavaField()).thenReturn(OWLClassP.getSimpleListField());
        when(et.getFieldSpecification(OWLClassP.getSimpleListField().getName())).thenReturn(simpleListAtt);
        when(simpleListAtt.isCollection()).thenReturn(true);
        when(simpleListAtt.getDeclaringType()).thenReturn(et);
        when(simpleListAtt.getCollectionType()).thenReturn(PluralAttribute.CollectionType.LIST);
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
        when(refListAtt.getCollectionType()).thenReturn(PluralAttribute.CollectionType.LIST);
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
    }

    public static void initOwlClassQMock(EntityType<OWLClassQ> et, SingularAttribute qStringAtt,
                                         SingularAttribute qParentStringAtt,
                                         SingularAttribute qLabelAtt,
                                         SingularAttribute qOwlClassAAtt, Identifier idQ) throws Exception {
        when(et.getIdentifier()).thenReturn(idQ);
        when(idQ.getJavaField()).thenReturn(OWLClassQ.getUriField());
        when(et.getIRI()).thenReturn(IRI.create(OWLClassQ.getClassIri()));
        when(et.getFieldSpecifications())
                .thenReturn(
                        new HashSet<>(
                                Arrays.<FieldSpecification<? super OWLClassQ, ?>>asList(qStringAtt, qParentStringAtt,
                                        qLabelAtt,
                                        qOwlClassAAtt)));
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
        when(qParentStringAtt.getDeclaringType()).thenReturn(et);
        when(et.getFieldSpecification(qParentStringAtt.getName())).thenReturn(qParentStringAtt);

        when(qLabelAtt.getJavaField()).thenReturn(OWLClassQ.getLabelField());
        when(qLabelAtt.getJavaType()).thenReturn(OWLClassQ.getLabelField().getType());
        when(qLabelAtt.getName()).thenReturn(OWLClassQ.getLabelField().getName());
        when(et.getAttribute(OWLClassQ.getLabelField().getName())).thenReturn(qLabelAtt);
        when(qLabelAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(qLabelAtt.isCollection()).thenReturn(false);
        when(qLabelAtt.getBindableJavaType()).thenReturn(String.class);
        when(qLabelAtt.getIRI()).thenReturn(
                IRI.create(OWLClassQ.getLabelField().getAnnotation(OWLAnnotationProperty.class).iri()));
        when(qLabelAtt.getDeclaringType()).thenReturn(et);
        when(et.getFieldSpecification(qLabelAtt.getName())).thenReturn(qLabelAtt);

        when(qOwlClassAAtt.getIRI())
                .thenReturn(IRI.create(OWLClassQ.getOwlClassAField().getAnnotation(OWLObjectProperty.class).iri()));
        when(qOwlClassAAtt.getJavaType()).thenReturn(OWLClassA.class);
        when(qOwlClassAAtt.getJavaField()).thenReturn(OWLClassQ.getOwlClassAField());
        when(qOwlClassAAtt.getName()).thenReturn(OWLClassQ.getOwlClassAField().getName());
        when(qOwlClassAAtt.getConstraints()).thenReturn(new ParticipationConstraint[]{});
        when(qOwlClassAAtt.getFetchType()).thenReturn(FetchType.EAGER);
        when(qOwlClassAAtt.getDeclaringType()).thenReturn(et);
        when(qOwlClassAAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(et.getFieldSpecification(qOwlClassAAtt.getName())).thenReturn(qOwlClassAAtt);
    }
}
