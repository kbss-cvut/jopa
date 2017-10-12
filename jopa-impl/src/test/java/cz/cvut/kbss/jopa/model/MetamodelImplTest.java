/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

@SuppressWarnings("unused")
public class MetamodelImplTest {

    private static final Map<String, String> PROPERTIES = Collections
            .singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa");

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private PersistenceUnitClassFinder classFinderMock;

    private Configuration conf = new Configuration(PROPERTIES);

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void buildsSingleEntityWithSingularDataProperty() throws Exception {
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
        assertTrue(attribute instanceof SingularAttributeImpl);
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
        assertTrue(Arrays.equals(cascadeTypes, att.getCascadeTypes()));
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
    public void buildsSingleEntityWithSingularObjectProperty() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassD.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassD> et = metamodel.entity(OWLClassD.class);
        assertFalse(et.getIdentifier().isGenerated());
        final FieldSpecification<? super OWLClassD, ?> att = et
                .getFieldSpecification(OWLClassD.getOwlClassAField().getName());
        assertNotNull(att);
        final Field expectedField = OWLClassD.getOwlClassAField();
        checkSingularAttribute(att, et, expectedField.getName(), Attribute.PersistentAttributeType.OBJECT,
                expectedField, FetchType.EAGER, false, expectedField.getAnnotation(OWLObjectProperty.class).iri(),
                expectedField.getType(), new CascadeType[]{});
    }

    @Test
    public void buildsCorrectIdentifierFromUriField() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassE.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassE> et = metamodel.entity(OWLClassE.class);
        final Identifier id = et.getIdentifier();
        assertTrue(id.isGenerated());
        assertEquals(OWLClassE.class.getDeclaredField("uri"), id.getJavaField());
    }

    @Test
    public void buildsSingleEntityWithMultipleSingularDataProperties() throws Exception {
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
                FetchType.EAGER, false, intField.getAnnotation(OWLDataProperty.class).iri(), intField.getType(),
                new CascadeType[]{});
        final Field longField = OWLClassM.getLongAttributeField();
        final FieldSpecification<? super OWLClassM, ?> longAtt = et.getFieldSpecification(longField.getName());
        checkSingularAttribute(longAtt, et, longField.getName(), Attribute.PersistentAttributeType.DATA, longField,
                FetchType.EAGER, false, longField.getAnnotation(OWLDataProperty.class).iri(), longField.getType(),
                new CascadeType[]{});
        final Field doubleField = OWLClassM.getDoubleAttributeField();
        final FieldSpecification<? super OWLClassM, ?> dAtt = et.getFieldSpecification(doubleField.getName());
        checkSingularAttribute(dAtt, et, doubleField.getName(), Attribute.PersistentAttributeType.DATA, doubleField,
                FetchType.EAGER, false, doubleField.getAnnotation(OWLDataProperty.class).iri(), doubleField.getType(),
                new CascadeType[]{});
        final Field dateField = OWLClassM.getDateAttributeField();
        final FieldSpecification<? super OWLClassM, ?> dateAtt = et.getFieldSpecification(dateField.getName());
        checkSingularAttribute(dateAtt, et, dateField.getName(), Attribute.PersistentAttributeType.DATA, dateField,
                FetchType.EAGER, false, dateField.getAnnotation(OWLDataProperty.class).iri(), dateField.getType(),
                new CascadeType[]{});
        final Field enumField = OWLClassM.getEnumAttributeField();
        final FieldSpecification<? super OWLClassM, ?> enumAtt = et.getFieldSpecification(enumField.getName());
        checkSingularAttribute(enumAtt, et, enumField.getName(), Attribute.PersistentAttributeType.DATA, enumField,
                FetchType.EAGER, false, enumField.getAnnotation(OWLDataProperty.class).iri(), enumField.getType(),
                new CascadeType[]{});
    }

    @Test
    public void buildsSingleEntityWithSetBasedObjectProperty() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassJ.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassJ> et = metamodel.entity(OWLClassJ.class);
        assertNotNull(et);
        final Field setField = OWLClassJ.getOwlClassAField();
        final FieldSpecification<? super OWLClassJ, ?> att = et.getFieldSpecification(setField.getName());
        assertNotNull(att);
        checkPluralAttribute(att, et, setField.getName(), Attribute.PersistentAttributeType.OBJECT, setField,
                FetchType.LAZY, false, setField.getAnnotation(OWLObjectProperty.class).iri(),
                OWLClassA.class, PluralAttribute.CollectionType.SET, new CascadeType[]{CascadeType.ALL});
    }

    private void checkPluralAttribute(FieldSpecification<?, ?> attribute, EntityType<?> declaringType, String name,
                                      Attribute.PersistentAttributeType type, Field field, FetchType fetch,
                                      boolean inferred, String iri, Class<?> bindableJavaType,
                                      PluralAttribute.CollectionType collectionType, CascadeType[] cascadeTypes) {
        assertTrue(attribute instanceof PluralAttribute);
        final PluralAttribute<?, ?, ?> pluralAttribute = (PluralAttribute<?, ?, ?>) attribute;
        assertTrue(pluralAttribute.isCollection());  // it is plural
        assertEquals(collectionType, pluralAttribute.getCollectionType());  // correct collection type
        assertEquals(Bindable.BindableType.PLURAL_ATTRIBUTE,
                pluralAttribute.getBindableType());    // correct bindable type
        assertEquals(bindableJavaType, pluralAttribute.getBindableJavaType());    // correct bindable java type
        checkCommonProperties(pluralAttribute, declaringType, name, type, field, fetch, inferred, iri, cascadeTypes);
    }

    @Test
    public void buildsSingleEntityWithListsSimpleList() throws Exception {
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
                OWLClassA.class, PluralAttribute.CollectionType.LIST, simpleListProperty.cascade(), SequenceType.simple,
                simpleListSequence.ClassOWLListIRI(), simpleListSequence.ObjectPropertyHasContentsIRI(),
                simpleListSequence.ObjectPropertyHasNextIRI());
    }

    private void checkPluralListAttribute(FieldSpecification<?, ?> attribute, EntityType<?> declaringType, String name,
                                          Attribute.PersistentAttributeType type, Field field, FetchType fetch,
                                          boolean inferred, String iri, Class<?> bindableJavaType,
                                          PluralAttribute.CollectionType collectionType, CascadeType[] cascadeTypes,
                                          SequenceType sequenceType, String owlListClass, String hasContents,
                                          String hasNext) {
        checkPluralAttribute(attribute, declaringType, name, type, field, fetch, inferred, iri, bindableJavaType,
                collectionType, cascadeTypes);
        assertTrue(attribute instanceof ListAttribute);
        final ListAttribute<?, ?> listAttribute = (ListAttribute<?, ?>) attribute;
        assertEquals(sequenceType, listAttribute.getSequenceType());
        assertEquals(owlListClass, listAttribute.getOWLListClass().toString());
        assertEquals(hasContents, listAttribute.getOWLPropertyHasContentsIRI().toString());
        assertEquals(hasNext, listAttribute.getOWLObjectPropertyHasNextIRI().toString());
    }

    @Test
    public void buildsSingleEntityWithListsReferencedList() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassC.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassC> et = metamodel.entity(OWLClassC.class);
        assertNotNull(et);
        final Field referencedListField = OWLClassC.getRefListField();
        final FieldSpecification<?, ?> referencedListAtt = et.getFieldSpecification(referencedListField.getName());
        final OWLObjectProperty referencedListProperty = referencedListField.getAnnotation(OWLObjectProperty.class);
        final Sequence referencedListSequence = referencedListField.getAnnotation(Sequence.class);
        checkPluralListAttribute(referencedListAtt, et, referencedListField.getName(),
                Attribute.PersistentAttributeType.OBJECT, referencedListField, referencedListProperty.fetch(), false,
                referencedListProperty.iri(), OWLClassA.class, PluralAttribute.CollectionType.LIST,
                referencedListProperty.cascade(), SequenceType.referenced, referencedListSequence.ClassOWLListIRI(),
                referencedListSequence.ObjectPropertyHasContentsIRI(),
                referencedListSequence.ObjectPropertyHasNextIRI());
    }

    @Test
    public void buildsSingleEntityWithTypesField() throws Exception {
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
    public void buildsSingleEntityWithPropertiesField() throws Exception {
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
    public void buildsSingleEntityWithParticipationConstraints() throws Exception {
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
    public void buildsSingleEntityWithAnnotationPropertyAndInferredPropertiesField() throws Exception {
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
                annotationField.getAnnotation(OWLAnnotationProperty.class).iri(), String.class, new CascadeType[]{});

        final PropertiesSpecification<?, ?, ?, ?> propsSpec = et.getProperties();
        assertNotNull(propsSpec);
        assertTrue(propsSpec.isInferred());
        assertTrue(metamodel.getInferredClasses().contains(OWLClassN.class));
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionWhenTryingToBuildClassWithoutOWLClassAnnotation() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(String.class));
        getMetamodel();
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionWhenTypesFieldIsNotASet() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithInvalidTypes.class));
        getMetamodel();
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithInvalidTypes")
    private static class ClassWithInvalidTypes {
        @Id
        private String id;
        @Types
        private List<String> types;
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionWhenPropertiesIsNotAMap() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithInvalidProperties.class));
        getMetamodel();
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithInvalidProperties")
    private static class ClassWithInvalidProperties {
        @Id
        private String id;
        @Properties
        private List<String> properties;
    }

    @Test(expected = InvalidFieldMappingException.class)
    public void throwExceptionWhenForClassWithInvalidIdentifier() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithInvalidIdentifier.class));
        getMetamodel();
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithInvalidIdentifier")
    private static class ClassWithInvalidIdentifier {
        @Id
        private Integer id;
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionForClassWithoutIdentifier() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithoutIdentifier.class));
        getMetamodel();
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithoutIdentifier")
    private static class ClassWithoutIdentifier {
        @Properties
        private Map<String, Set<String>> properties;
    }

    @Test
    public void buildsSingleEntityWithSingularDataPropertyWithNonEmptyField() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(OWLClassN.class));
        final Metamodel metamodel = getMetamodel();

        final EntityType<OWLClassN> et = metamodel.entity(OWLClassN.class);
        final Field strAttField = OWLClassN.getStringAttributeField();
        final FieldSpecification<? super OWLClassN, ?> att = et.getFieldSpecification(strAttField.getName());
        assertTrue(att instanceof SingularAttribute);
        assertTrue(((SingularAttribute) att).isNonEmpty());
    }

    @Test
    public void skipsStaticFieldsWhenProcessingClass() throws Exception {
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
    public void skipsFieldsAnnotatedWithTransientWhenProcessingClass() throws Exception {
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
    public void skipsTransientFieldsWhenProcessingClass() throws Exception {
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
    public void skipsFinalFieldsWhenProcessingClass() throws Exception {
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

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionForEntityWithoutNoArgConstructor() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithoutNoArgConstructor.class));

        getMetamodel();
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithoutNoArgConstructor")
    private static class ClassWithoutNoArgConstructor {

        @Id
        private URI id;

        public ClassWithoutNoArgConstructor(URI id) {
            this.id = id;
        }
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionForEntityWithPersistentArray() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithArrayAttribute.class));

        getMetamodel();
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithArrayAttribute")
    private static class ClassWithArrayAttribute {

        @Id
        private URI id;

        @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa#objectProperty")
        private OWLClassA[] arr;
    }

    @Test
    public void buildsEntityWithObjectPropertyAttributeAsUri() throws Exception {
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

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithOPUri")
    private static class ClassWithOPUri {

        @Id
        private URI id;

        @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa#objectProperty")
        private URI op;
    }

    @Test
    public void buildsEntityWithPluralObjectPropertyAttributeAsUrls() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithPluralOPUrls.class));

        final Metamodel m = getMetamodel();
        final EntityType<ClassWithPluralOPUrls> et = m.entity(ClassWithPluralOPUrls.class);
        assertNotNull(et);
        final Attribute<? super ClassWithPluralOPUrls, ?> att = et.getAttribute("op");
        assertNotNull(att);
        assertTrue(att.isCollection());
        assertTrue(att instanceof PluralAttribute);
        assertEquals(Attribute.PersistentAttributeType.OBJECT, att.getPersistentAttributeType());
        assertArrayEquals(new CascadeType[0], att.getCascadeTypes());
        assertEquals(URL.class, ((PluralAttribute) att).getBindableJavaType());
        assertEquals(PluralAttribute.CollectionType.SET, ((PluralAttribute) att).getCollectionType());
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithOPUri")
    private static class ClassWithPluralOPUrls {

        @Id
        private URI id;

        @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa#objectProperty")
        private Set<URL> op;
    }

    @Test
    public void buildsEntityWithUriTypesField() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithUriTypes.class));

        final Metamodel m = getMetamodel();
        final EntityType<ClassWithUriTypes> et = m.entity(ClassWithUriTypes.class);
        assertNotNull(et);
        assertEquals(URI.class, et.getTypes().getElementType());
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithUriTypes")
    private static class ClassWithUriTypes {
        @Id
        private URI id;

        @Types
        private Set<URI> types;
    }

    @Test
    public void entityThrowsIllegalArgumentForNonEntityClass() throws Exception {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Class " + ClassWithUriTypes.class.getName() + " is not a known entity in this persistence unit.");

        final Metamodel m = getMetamodel();
        m.entity(ClassWithUriTypes.class);
    }

    @Test
    public void buildsEntityWithNamedQueries() throws Exception {
        when(classFinderMock.getEntities()).thenReturn(Collections.singleton(ClassWithNamedQueries.class));
        final MetamodelImpl metamodel = getMetamodel();
        final NamedQueryManager queryManager = metamodel.getNamedQueryManager();
        assertNotNull(queryManager.getQuery("selectAll"));
        assertNotNull(queryManager.getQuery("askQuery"));
    }

    @NamedNativeQueries({
            @NamedNativeQuery(name = "selectAll", query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z . }"),
            @NamedNativeQuery(name = "askQuery", query = "ASK WHERE { ?x a ?type . }")
    })
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithNamedQueries")
    private static class ClassWithNamedQueries {
        @Id
        private URI id;

        @Types
        private Set<String> types;
    }

    @Test
    public void getManagedTypesReturnsEntitiesAndMappedSuperclasses() {
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
    public void getEntitiesReturnsOnlyEntityTypes() {
        when(classFinderMock.getEntities()).thenReturn(new HashSet<>(Arrays.asList(OWLClassQ.class, OWLClassA.class)));
        final MetamodelImpl metamodel = getMetamodel();
        final Set<EntityType<?>> entities = metamodel.getEntities();
        assertEquals(2, entities.size());
        final Set<Class<?>> types = entities.stream().map(Type::getJavaType).collect(Collectors.toSet());
        assertTrue(types.contains(OWLClassQ.class));
        assertTrue(types.contains(OWLClassA.class));
    }

    @Test
    public void getModuleExtractionSignatureExtractsUrisFromConfiguration() {
        final Set<String> signature = Generators.generateTypes(5);
        conf.set(OntoDriverProperties.MODULE_EXTRACTION_SIGNATURE, String.join("|", signature));

        final MetamodelImpl metamodel = getMetamodel();
        final Set<URI> result = metamodel.getModuleExtractionExtraSignature();
        assertEquals(signature.size(), result.size());
        result.forEach(uri -> assertTrue(signature.contains(uri.toString())));
    }

    @Test
    public void getModuleExtractionSignatureReturnsEmptyCollectionForNoSignature() {
        final MetamodelImpl metamodel = getMetamodel();
        assertTrue(metamodel.getModuleExtractionExtraSignature().isEmpty());
    }

    @Test
    public void addingUriToModuleExtractionSignatureReflectsInFutureInvocationOfGetModuleExtractionSignature() {
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
}
