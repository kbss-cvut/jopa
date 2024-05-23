/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.TestLocal;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxy;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.typeCompatibleWith;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("unused")
class MetamodelImplTest {

    private static final Map<String, String> PROPERTIES = Collections
            .singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa");

    @Mock
    private PersistenceUnitClassFinder classFinderMock;

    private final Configuration conf = new Configuration(PROPERTIES);

    @Test
    void buildsSingleEntityWithSingularDataProperty() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassE.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassE> et = metamodel.entity(OWLClassE.class);
        assertNotNull(et);
        assertTrue(et.getIdentifier().isGenerated());
        final FieldSpecification<? super OWLClassE, ?> strAtt = et
                .getFieldSpecification(OWLClassE.getStrAttField().getName());
        assertNotNull(strAtt);
        final Field expectedField = OWLClassE.getStrAttField();
        checkSingularAttribute(strAtt, et, expectedField.getName(), Attribute.PersistentAttributeType.DATA,
                               expectedField,
                               FetchType.EAGER, false, expectedField.getAnnotation(OWLDataProperty.class).iri(),
                               expectedField.getType(), new CascadeType[]{});
    }

    private MetamodelImpl getMetamodel() {
        final MetamodelImpl metamodel = new MetamodelImpl(conf);
        metamodel.build(classFinderMock);
        return metamodel;
    }

    private void checkSingularAttribute(FieldSpecification<?, ?> attribute, EntityType<?> declaringType, String name,
                                        Attribute.PersistentAttributeType type, Field field, FetchType fetch,
                                        boolean inferred, String iri, Class<?> bindableJavaType,
                                        CascadeType[] cascadeTypes) {
        assertThat(attribute, instanceOf(SingularAttribute.class));
        final SingularAttribute<?, ?> singularAttribute = (SingularAttribute<?, ?>) attribute;
        assertFalse(singularAttribute.isId());  // it is not an id
        assertFalse(singularAttribute.isCollection());  // it is singular
        assertEquals(Bindable.BindableType.SINGULAR_ATTRIBUTE,
                     singularAttribute.getBindableType());    // correct bindable type
        assertEquals(bindableJavaType, singularAttribute.getBindableJavaType());    // correct bindable java type
        checkCommonProperties(singularAttribute, declaringType, name, type, field, fetch, inferred, iri, cascadeTypes);
    }

    private void checkCommonProperties(Attribute<?, ?> att, EntityType<?> declaringType, String name,
                                       Attribute.PersistentAttributeType type, Field field, FetchType fetch,
                                       boolean inferred, String iri, CascadeType[] cascadeTypes) {
        assertEquals(type, att.getPersistentAttributeType()); // correct type (data, object, annotation)
        assertEquals(iri, att.getIRI().toString());
        assertArrayEquals(cascadeTypes, att.getCascadeTypes());
        checkBasicProperties(att, declaringType, name, field, fetch, inferred);
    }

    private void checkBasicProperties(FieldSpecification<?, ?> att, EntityType<?> declaringType, String name,
                                      Field field, FetchType fetch,
                                      boolean inferred) {
        assertEquals(name, att.getName());    // correct attribute name
        assertEquals(field, att.getJavaField());  // correct java field
        assertEquals(fetch, att.getFetchType());  // correct fetch type
        assertEquals(inferred, att.isInferred()); // correct inferred support
        assertEquals(declaringType, att.getDeclaringType());
    }

    @Test
    void buildsSingleEntityWithSingularObjectProperty() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassD.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassD> et = metamodel.entity(OWLClassD.class);
        assertFalse(et.getIdentifier().isGenerated());
        final FieldSpecification<? super OWLClassD, ?> att = et
                .getFieldSpecification(OWLClassD.getOwlClassAField().getName());
        assertNotNull(att);
        final Field expectedField = OWLClassD.getOwlClassAField();
        checkSingularAttribute(att, et, expectedField.getName(), Attribute.PersistentAttributeType.OBJECT,
                               expectedField, FetchType.EAGER, false,
                               expectedField.getAnnotation(OWLObjectProperty.class).iri(),
                               expectedField.getType(), new CascadeType[]{});
    }

    @Test
    void buildsCorrectIdentifierFromUriField() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassE.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassE> et = metamodel.entity(OWLClassE.class);
        final Identifier<? super OWLClassE, ?> id = et.getIdentifier();
        assertTrue(id.isGenerated());
        assertEquals(OWLClassE.class.getDeclaredField("uri"), id.getJavaField());
    }

    @Test
    void buildsSingleEntityWithMultipleSingularDataProperties() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassM.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassM> et = metamodel.entity(OWLClassM.class);
        assertNotNull(et);
        final Field booleanField = OWLClassM.getBooleanAttributeField();
        final FieldSpecification<? super OWLClassM, ?> bAtt = et.getFieldSpecification(booleanField.getName());
        checkSingularAttribute(bAtt, et, booleanField.getName(), Attribute.PersistentAttributeType.DATA, booleanField,
                               FetchType.EAGER, false, booleanField.getAnnotation(OWLDataProperty.class).iri(),
                               booleanField.getType(), new CascadeType[]{});
        final Field intField = OWLClassM.getIntAttributeField();
        final FieldSpecification<? super OWLClassM, ?> iAtt = et.getFieldSpecification(intField.getName());
        checkSingularAttribute(iAtt, et, intField.getName(), Attribute.PersistentAttributeType.DATA, intField,
                               FetchType.EAGER, false, intField.getAnnotation(OWLDataProperty.class).iri(),
                               intField.getType(),
                               new CascadeType[]{});
        final Field longField = OWLClassM.getLongAttributeField();
        final FieldSpecification<? super OWLClassM, ?> longAtt = et.getFieldSpecification(longField.getName());
        checkSingularAttribute(longAtt, et, longField.getName(), Attribute.PersistentAttributeType.DATA, longField,
                               FetchType.EAGER, false, longField.getAnnotation(OWLDataProperty.class).iri(),
                               longField.getType(),
                               new CascadeType[]{});
        final Field doubleField = OWLClassM.getDoubleAttributeField();
        final FieldSpecification<? super OWLClassM, ?> dAtt = et.getFieldSpecification(doubleField.getName());
        checkSingularAttribute(dAtt, et, doubleField.getName(), Attribute.PersistentAttributeType.DATA, doubleField,
                               FetchType.EAGER, false, doubleField.getAnnotation(OWLDataProperty.class).iri(),
                               doubleField.getType(),
                               new CascadeType[]{});
        final Field dateField = OWLClassM.getDateAttributeField();
        final FieldSpecification<? super OWLClassM, ?> dateAtt = et.getFieldSpecification(dateField.getName());
        checkSingularAttribute(dateAtt, et, dateField.getName(), Attribute.PersistentAttributeType.DATA, dateField,
                               FetchType.EAGER, false, dateField.getAnnotation(OWLDataProperty.class).iri(),
                               dateField.getType(),
                               new CascadeType[]{});
        final Field enumField = OWLClassM.getEnumAttributeField();
        final FieldSpecification<? super OWLClassM, ?> enumAtt = et.getFieldSpecification(enumField.getName());
        checkSingularAttribute(enumAtt, et, enumField.getName(), Attribute.PersistentAttributeType.DATA, enumField,
                               FetchType.EAGER, false, enumField.getAnnotation(OWLDataProperty.class).iri(),
                               enumField.getType(),
                               new CascadeType[]{});
    }

    @Test
    void buildsSingleEntityWithSetBasedObjectProperty() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassJ.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassJ> et = metamodel.entity(OWLClassJ.class);
        assertNotNull(et);
        final Field setField = OWLClassJ.getOwlClassAField();
        final FieldSpecification<? super OWLClassJ, ?> att = et.getFieldSpecification(setField.getName());
        assertNotNull(att);
        checkPluralAttribute(att, et, setField.getName(), Attribute.PersistentAttributeType.OBJECT, setField,
                             FetchType.LAZY, false, setField.getAnnotation(OWLObjectProperty.class).iri(),
                             OWLClassA.class, CollectionType.SET, new CascadeType[]{CascadeType.ALL});
    }

    private void checkPluralAttribute(FieldSpecification<?, ?> attribute, EntityType<?> declaringType, String name,
                                      Attribute.PersistentAttributeType type, Field field, FetchType fetch,
                                      boolean inferred, String iri, Class<?> bindableJavaType,
                                      CollectionType collectionType, CascadeType[] cascadeTypes) {
        assertThat(attribute, instanceOf(PluralAttribute.class));
        final PluralAttribute<?, ?, ?> pluralAttribute = (PluralAttribute<?, ?, ?>) attribute;
        assertTrue(pluralAttribute.isCollection());  // it is plural
        assertEquals(collectionType, pluralAttribute.getCollectionType());  // correct collection type
        assertEquals(Bindable.BindableType.PLURAL_ATTRIBUTE,
                     pluralAttribute.getBindableType());    // correct bindable type
        assertEquals(bindableJavaType, pluralAttribute.getBindableJavaType());    // correct bindable java type
        checkCommonProperties(pluralAttribute, declaringType, name, type, field, fetch, inferred, iri, cascadeTypes);
    }

    @Test
    void buildsSingleEntityWithListsSimpleList() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassC.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassC> et = metamodel.entity(OWLClassC.class);
        assertNotNull(et);
        final Field simpleListField = OWLClassC.getSimpleListField();
        final FieldSpecification<?, ?> simpleListAtt = et.getFieldSpecification(simpleListField.getName());
        final OWLObjectProperty simpleListProperty = simpleListField.getAnnotation(OWLObjectProperty.class);
        final Sequence simpleListSequence = simpleListField.getAnnotation(Sequence.class);
        checkPluralListAttribute(simpleListAtt, et, simpleListField.getName(), Attribute.PersistentAttributeType.OBJECT,
                                 simpleListField, simpleListProperty.fetch(), false, simpleListProperty.iri(),
                                 OWLClassA.class, CollectionType.LIST, simpleListProperty.cascade(),
                                 SequenceType.simple,
                                 simpleListSequence.listClassIRI(),
                                 simpleListSequence.hasContentsPropertyIRI(),
                                 simpleListSequence.hasNextPropertyIRI());
    }

    private void checkPluralListAttribute(FieldSpecification<?, ?> attribute, EntityType<?> declaringType, String name,
                                          Attribute.PersistentAttributeType type, Field field, FetchType fetch,
                                          boolean inferred, String iri, Class<?> bindableJavaType,
                                          CollectionType collectionType, CascadeType[] cascadeTypes,
                                          SequenceType sequenceType, String owlListClass, String hasContents,
                                          String hasNext) {
        checkPluralAttribute(attribute, declaringType, name, type, field, fetch, inferred, iri, bindableJavaType,
                             collectionType, cascadeTypes);
        assertThat(attribute, instanceOf(ListAttribute.class));
        final ListAttribute<?, ?> listAttribute = (ListAttribute<?, ?>) attribute;
        assertEquals(sequenceType, listAttribute.getSequenceType());
        assertEquals(owlListClass, listAttribute.getListClassIRI().toString());
        assertEquals(hasContents, listAttribute.getHasContentsPropertyIRI().toString());
        assertEquals(hasNext, listAttribute.getHasNextPropertyIRI().toString());
    }

    @Test
    void buildsSingleEntityWithListsReferencedList() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassC.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassC> et = metamodel.entity(OWLClassC.class);
        assertNotNull(et);
        final Field referencedListField = OWLClassC.getRefListField();
        final FieldSpecification<?, ?> referencedListAtt = et.getFieldSpecification(referencedListField.getName());
        final OWLObjectProperty referencedListProperty = referencedListField.getAnnotation(OWLObjectProperty.class);
        final Sequence referencedListSequence = referencedListField.getAnnotation(Sequence.class);
        checkPluralListAttribute(referencedListAtt, et, referencedListField.getName(),
                                 Attribute.PersistentAttributeType.OBJECT, referencedListField,
                                 referencedListProperty.fetch(), false,
                                 referencedListProperty.iri(), OWLClassA.class, CollectionType.LIST,
                                 referencedListProperty.cascade(), SequenceType.referenced,
                                 referencedListSequence.listClassIRI(),
                                 referencedListSequence.hasContentsPropertyIRI(),
                                 referencedListSequence.hasNextPropertyIRI());
    }

    @Test
    void buildsSingleEntityWithTypesField() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassA.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        assertNotNull(et);
        final Field typesField = OWLClassA.getTypesField();
        final TypesSpecification<? super OWLClassA, ?> typesSpec = et.getTypes();
        assertNotNull(typesSpec);
        checkBasicProperties(typesSpec, et, typesField.getName(), typesField, FetchType.EAGER, false);
        assertEquals(Set.class, typesSpec.getJavaType());
    }

    @Test
    void buildsSingleEntityWithPropertiesField() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassB.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassB> et = metamodel.entity(OWLClassB.class);
        assertNotNull(et);
        final Field propertiesField = OWLClassB.getPropertiesField();
        final PropertiesSpecification<? super OWLClassB, ?, ?, ?> propertiesSpec = et.getProperties();
        assertNotNull(propertiesSpec);
        checkBasicProperties(propertiesSpec, et, propertiesField.getName(), propertiesField, FetchType.LAZY, false);
        assertEquals(Map.class, propertiesSpec.getJavaType());
    }

    @Test
    void buildsSingleEntityWithParticipationConstraints() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassL.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassL> et = metamodel.entity(OWLClassL.class);
        assertNotNull(et);
        final Field setField = OWLClassL.getSetField();
        final FieldSpecification<? super OWLClassL, ?> setSpec = et.getFieldSpecification(setField.getName());
        assertNotNull(setSpec);
        final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) setSpec;
        final ParticipationConstraint fieldConstraint = setField.getAnnotation(ParticipationConstraints.class)
                                                                .value()[0];
        final ParticipationConstraint[] constraints = pa.getConstraints();
        assertEquals(1, constraints.length);
        assertEquals(fieldConstraint.min(), constraints[0].min());
        assertEquals(fieldConstraint.max(), constraints[0].max());
        assertEquals(fieldConstraint.owlObjectIRI(), constraints[0].owlObjectIRI());
    }

    @Test
    void buildsSingleEntityWithAnnotationPropertyAndInferredPropertiesField() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassN.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassN> et = metamodel.entity(OWLClassN.class);
        assertNotNull(et);
        final Field annotationField = OWLClassN.getAnnotationPropertyField();
        final FieldSpecification<? super OWLClassN, ?> annotationProperty = et
                .getFieldSpecification(annotationField.getName());
        assertNotNull(annotationProperty);
        checkSingularAttribute(annotationProperty, et, annotationField.getName(),
                               Attribute.PersistentAttributeType.ANNOTATION, annotationField, FetchType.EAGER, false,
                               annotationField.getAnnotation(OWLAnnotationProperty.class).iri(), String.class,
                               new CascadeType[]{});

        final PropertiesSpecification<?, ?, ?, ?> propsSpec = et.getProperties();
        assertNotNull(propsSpec);
        assertTrue(propsSpec.isInferred());
        assertTrue(metamodel.getInferredClasses().contains(OWLClassN.class));
    }

    @Test
    void throwsExceptionWhenTryingToBuildClassWithoutOWLClassAnnotation() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(String.class));
        assertThrows(MetamodelInitializationException.class, this::getMetamodel);
    }

    @Test
    void throwsExceptionWhenTypesFieldIsNotASet() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithInvalidTypes.class));
        assertThrows(MetamodelInitializationException.class, this::getMetamodel);
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithInvalidTypes")
    public static class ClassWithInvalidTypes {
        @Id
        private String id;
        @Types
        private List<String> types;
    }

    @Test
    void throwsExceptionWhenPropertiesIsNotAMap() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithInvalidProperties.class));
        assertThrows(MetamodelInitializationException.class, this::getMetamodel);
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithInvalidProperties")
    public static class ClassWithInvalidProperties {
        @Id
        private String id;
        @Properties
        private List<String> properties;
    }

    @Test
    void throwExceptionWhenForClassWithInvalidIdentifier() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithInvalidIdentifier.class));
        assertThrows(MetamodelInitializationException.class, this::getMetamodel);
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithInvalidIdentifier")
    public static class ClassWithInvalidIdentifier {
        @Id
        private Integer id;
    }

    @Test
    void throwsExceptionForClassWithoutIdentifier() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithoutIdentifier.class));
        assertThrows(MetamodelInitializationException.class, this::getMetamodel);
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithoutIdentifier")
    public static class ClassWithoutIdentifier {
        @Properties
        private Map<String, Set<String>> properties;
    }

    @Test
    void buildsSingleEntityWithSingularDataPropertyWithNonEmptyField() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassN.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassN> et = metamodel.entity(OWLClassN.class);
        final Field strAttField = OWLClassN.getStringAttributeField();
        final FieldSpecification<? super OWLClassN, ?> att = et.getFieldSpecification(strAttField.getName());
        assertThat(att, instanceOf(SingularAttribute.class));
        assertTrue(((SingularAttribute<?, ?>) att).isNonEmpty());
    }

    @Test
    void skipsStaticFieldsWhenProcessingClass() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassO.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassO> et = metamodel.entity(OWLClassO.class);
        assertEquals(1, et.getDeclaredAttributes().size());
        try {
            et.getFieldSpecification("STR_ATT_FIELD");
        } catch (IllegalArgumentException e) {
            // Do nothing, this is correct
        }
        try {
            et.getFieldSpecification("primitiveField");
        } catch (IllegalArgumentException e) {
            // Do nothing, this is correct
        }
    }

    @Test
    void skipsFieldsAnnotatedWithTransientWhenProcessingClass() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassO.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassO> et = metamodel.entity(OWLClassO.class);
        assertEquals(1, et.getDeclaredAttributes().size());
        try {
            et.getFieldSpecification(OWLClassO.TRANSIENT_ANNOTATED_FIELD_NAME);
        } catch (IllegalArgumentException e) {
            // Do nothing, this is correct
        }
    }

    @Test
    void skipsTransientFieldsWhenProcessingClass() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassO.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassO> et = metamodel.entity(OWLClassO.class);
        assertEquals(1, et.getDeclaredAttributes().size());
        try {
            et.getFieldSpecification(OWLClassO.TRANSIENT_FIELD_NAME);
        } catch (IllegalArgumentException e) {
            // Do nothing, this is correct
        }
    }

    @Test
    void skipsFinalFieldsWhenProcessingClass() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassO.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassO> et = metamodel.entity(OWLClassO.class);
        assertEquals(1, et.getDeclaredAttributes().size());
        try {
            et.getFieldSpecification(OWLClassO.TRANSIENT_FINAL_FIELD_NAME);
        } catch (IllegalArgumentException e) {
            // Do nothing, this is correct
        }
    }

    @Test
    void throwsExceptionForEntityWithoutNoArgConstructor() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithoutNoArgConstructor.class));
        assertThrows(MetamodelInitializationException.class, this::getMetamodel);
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithoutNoArgConstructor")
    public static class ClassWithoutNoArgConstructor {

        @Id
        private URI id;

        public ClassWithoutNoArgConstructor(URI id) {
            this.id = id;
        }
    }

    @Test
    void throwsExceptionForEntityWithPersistentArray() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithArrayAttribute.class));
        assertThrows(MetamodelInitializationException.class, this::getMetamodel);
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithArrayAttribute")
    public static class ClassWithArrayAttribute {

        @Id
        private URI id;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "objectProperty")
        private OWLClassA[] arr;
    }

    @Test
    void buildsEntityWithObjectPropertyAttributeAsUri() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithOPUri.class));

        final Metamodel m = getMetamodel();
        final EntityType<ClassWithOPUri> et = m.entity(ClassWithOPUri.class);
        assertNotNull(et);
        final Attribute<? super ClassWithOPUri, ?> att = et.getAttribute("op");
        assertNotNull(att);
        assertEquals(URI.class, att.getJavaType());
        assertEquals(Attribute.PersistentAttributeType.OBJECT, att.getPersistentAttributeType());
        assertArrayEquals(new CascadeType[0], att.getCascadeTypes());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithOPUri")
    public static class ClassWithOPUri {

        @Id
        private URI id;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "objectProperty")
        private URI op;
    }

    @Test
    void buildsEntityWithPluralObjectPropertyAttributeAsUrls() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithPluralOPUrls.class));

        final Metamodel m = getMetamodel();
        final EntityType<ClassWithPluralOPUrls> et = m.entity(ClassWithPluralOPUrls.class);
        assertNotNull(et);
        final Attribute<? super ClassWithPluralOPUrls, ?> att = et.getAttribute("op");
        assertNotNull(att);
        assertTrue(att.isCollection());
        assertThat(att, instanceOf(PluralAttribute.class));
        assertEquals(Attribute.PersistentAttributeType.OBJECT, att.getPersistentAttributeType());
        assertArrayEquals(new CascadeType[0], att.getCascadeTypes());
        assertEquals(URL.class, ((PluralAttribute<?, ?, ?>) att).getBindableJavaType());
        assertEquals(CollectionType.SET, ((PluralAttribute<?, ?, ?>) att).getCollectionType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithOPUri")
    public static class ClassWithPluralOPUrls {

        @Id
        private URI id;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "objectProperty")
        private Set<URL> op;
    }

    @Test
    void buildsEntityWithUriTypesField() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithUriTypes.class));

        final Metamodel m = getMetamodel();
        final EntityType<ClassWithUriTypes> et = m.entity(ClassWithUriTypes.class);
        assertNotNull(et);
        assertEquals(URI.class, et.getTypes().getElementType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithUriTypes")
    public static class ClassWithUriTypes {
        @Id
        private URI id;

        @Types
        private Set<URI> types;
    }

    @Test
    void entityThrowsIllegalArgumentForNonEntityClass() {
        final Metamodel m = getMetamodel();
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                                                         () -> m.entity(ClassWithUriTypes.class));
        assertThat(ex.getMessage(), containsString(
                ClassWithUriTypes.class.getName() + " is not a known entity in this persistence unit."));
    }

    @Test
    void buildsEntityWithNamedQueries() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithNamedQueries.class));
        final MetamodelImpl metamodel = getMetamodel();
        final NamedQueryManager queryManager = metamodel.getNamedQueryManager();
        assertNotNull(queryManager.getQuery("selectAll"));
        assertNotNull(queryManager.getQuery("askQuery"));
    }

    @TestLocal
    @NamedNativeQueries({
            @NamedNativeQuery(name = "selectAll", query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z . }"),
            @NamedNativeQuery(name = "askQuery", query = "ASK WHERE { ?x a ?type . }")
    })
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithNamedQueries")
    public static class ClassWithNamedQueries {
        @Id
        private URI id;

        @Types
        private Set<String> types;
    }

    @Test
    void getManagedTypesReturnsEntitiesAndMappedSuperclasses() {
        when(classFinderMock.getEntities()).thenReturn(new HashSet<>(Arrays.asList(OWLClassQ.class, OWLClassA.class)));
        final MetamodelImpl metamodel = getMetamodel();
        final Set<ManagedType<?>> managedTypes = metamodel.getManagedTypes();
        assertEquals(3, metamodel.getManagedTypes().size());
        final Set<Class<?>> types = managedTypes.stream().map(Type::getJavaType).collect(Collectors.toSet());
        assertTrue(types.contains(OWLClassA.class));
        assertTrue(types.contains(OWLClassQ.class));
        assertTrue(types.contains(QMappedSuperclass.class));
    }

    @Test
    void getEntitiesReturnsOnlyEntityTypes() {
        when(classFinderMock.getEntities()).thenReturn(new HashSet<>(Arrays.asList(OWLClassQ.class, OWLClassA.class)));
        final MetamodelImpl metamodel = getMetamodel();
        final Set<EntityType<?>> entities = metamodel.getEntities();
        assertEquals(2, entities.size());
        final Set<Class<?>> types = entities.stream().map(Type::getJavaType).collect(Collectors.toSet());
        assertTrue(types.contains(OWLClassQ.class));
        assertTrue(types.contains(OWLClassA.class));
    }

    @Test
    void getModuleExtractionSignatureExtractsUrisFromConfiguration() {
        final Set<String> signature = Generators.generateTypes(5);
        conf.set(OntoDriverProperties.MODULE_EXTRACTION_SIGNATURE, String.join("|", signature));

        final MetamodelImpl metamodel = getMetamodel();
        final Set<URI> result = metamodel.getModuleExtractionExtraSignature();
        assertEquals(signature.size(), result.size());
        result.forEach(uri -> assertTrue(signature.contains(uri.toString())));
    }

    @Test
    void getModuleExtractionSignatureReturnsEmptyCollectionForNoSignature() {
        final MetamodelImpl metamodel = getMetamodel();
        assertTrue(metamodel.getModuleExtractionExtraSignature().isEmpty());
    }

    @Test
    void addingUriToModuleExtractionSignatureReflectsInFutureInvocationOfGetModuleExtractionSignature() {
        final Set<String> signature = Generators.generateTypes(5);
        conf.set(OntoDriverProperties.MODULE_EXTRACTION_SIGNATURE, String.join("|", signature));

        final MetamodelImpl metamodel = getMetamodel();
        Set<URI> result = metamodel.getModuleExtractionExtraSignature();
        assertEquals(signature.size(), result.size());
        final URI toAdd = Generators.createIndividualIdentifier();
        metamodel.addUriToModuleExtractionSignature(toAdd);
        result = metamodel.getModuleExtractionExtraSignature();
        assertEquals(signature.size() + 1, result.size());
        assertTrue(result.contains(toAdd));
    }

    @Test
    void isEntityTypeReturnsTrueForEntityClass() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassE.class));
        final MetamodelImpl metamodel = getMetamodel();
        assertTrue(metamodel.isEntityType(OWLClassE.class));
    }

    @Test
    void isEntityTypeReturnsFalseForMappedSuperclass() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassQ.class));
        final MetamodelImpl metamodel = getMetamodel();
        assertFalse(metamodel.isEntityType(QMappedSuperclass.class));
    }

    @Test
    void getMappedEntitiesReturnsSetOfEntityTypesMappingSpecifiedClassIri() {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassA.class));
        final MetamodelImpl metamodel = getMetamodel();
        final Set<EntityType<?>> result = metamodel.getMappedEntities(Vocabulary.c_OwlClassA);
        assertEquals(Collections.singleton(metamodel.entity(OWLClassA.class)), result);
    }

    @Test
    void buildForSetOfClassesBuildsOnlyEntityClassesMetamodel() {
        final Set<Class<?>> entityClasses = new HashSet<>(Arrays.asList(OWLClassA.class, OWLClassE.class));
        final MetamodelImpl result = new MetamodelImpl(conf);
        result.build(entityClasses);
        entityClasses.forEach(cls -> {
            final EntityType<?> et = result.entity(cls);
            assertNotNull(et);
            assertEquals(cls, et.getJavaType());
        });
    }

    @Test
    void getLazyLoadingProxyReturnsLazyLoadingProxyClassForSpecifiedType() {
        final Set<Class<?>> entityClasses = new HashSet<>(List.of(OWLClassA.class));
        final MetamodelImpl sut = new MetamodelImpl(conf);
        sut.build(entityClasses);
        final Class<? extends OWLClassA> result = sut.getLazyLoadingProxy(OWLClassA.class);
        assertNotNull(result);
        assertThat(result, typeCompatibleWith(OWLClassA.class));
    }

    @Test
    void getLazyLoadingProxyCachesGeneratedProxyClasses() {
        final Set<Class<?>> entityClasses = new HashSet<>(List.of(OWLClassA.class));
        final MetamodelImpl sut = new MetamodelImpl(conf);
        sut.build(entityClasses);
        final Class<? extends OWLClassA> resultOne = sut.getLazyLoadingProxy(OWLClassA.class);
        final Class<? extends OWLClassA> resultTwo = sut.getLazyLoadingProxy(OWLClassA.class);
        assertSame(resultOne, resultTwo);
    }
}
