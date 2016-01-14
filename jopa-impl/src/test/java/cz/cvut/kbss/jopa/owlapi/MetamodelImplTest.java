package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

/**
 * @author ledvima1
 */
public class MetamodelImplTest {

    private static final Map<String, String> PROPERTIES = Collections
            .singletonMap(OWLAPIPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa");

    @Mock
    private EntityLoader entityLoaderMock;

    private Configuration conf = new Configuration(PROPERTIES);

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void buildsSingleEntityWithSingularDataProperty() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassE.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

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

        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassD.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

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
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassE.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassE> et = metamodel.entity(OWLClassE.class);
        final Identifier id = et.getIdentifier();
        assertTrue(id.isGenerated());
        assertEquals(OWLClassE.class.getDeclaredField("uri"), id.getJavaField());
    }

    @Test
    public void buildsSingleEntityWithMultipleSingularDataProperties() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassM.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

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
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassJ.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

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
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassC.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

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
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassC.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

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
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassA.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        assertNotNull(et);
        final Field typesField = OWLClassA.getTypesField();
        final TypesSpecification<? super OWLClassA, ?> typesSpec = et.getTypes();
        assertNotNull(typesSpec);
        checkBasicProperties(typesSpec, et, typesField.getName(), typesField, FetchType.EAGER, false);
    }

    @Test
    public void buildsSingleEntityWithPropertiesField() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassB.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassB> et = metamodel.entity(OWLClassB.class);
        assertNotNull(et);
        final Field propertiesField = OWLClassB.getPropertiesField();
        final PropertiesSpecification<? super OWLClassB, ?> propertiesSpec = et.getProperties();
        assertNotNull(propertiesSpec);
        checkBasicProperties(propertiesSpec, et, propertiesField.getName(), propertiesField, FetchType.LAZY, false);
    }

    @Test
    public void buildsSingleEntityWithParticipationConstraints() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassL.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

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
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassN.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassN> et = metamodel.entity(OWLClassN.class);
        assertNotNull(et);
        final Field annotationField = OWLClassN.getAnnotationPropertyField();
        final FieldSpecification<? super OWLClassN, ?> annotationProperty = et
                .getFieldSpecification(annotationField.getName());
        assertNotNull(annotationProperty);
        checkSingularAttribute(annotationProperty, et, annotationField.getName(),
                Attribute.PersistentAttributeType.ANNOTATION, annotationField, FetchType.EAGER, false,
                annotationField.getAnnotation(OWLAnnotationProperty.class).iri(), String.class, new CascadeType[]{});

        final PropertiesSpecification<?, ?> propsSpec = et.getProperties();
        assertNotNull(propsSpec);
        assertTrue(propsSpec.isInferred());
        assertTrue(metamodel.getInferredClasses().contains(OWLClassN.class));
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionWhenTryingToBuildClassWithoutOWLClassAnnotation() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(String.class));
        new MetamodelImpl(conf, entityLoaderMock);
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionWhenTypesFieldIsNotASet() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf))
                .thenReturn(Collections.singleton(ClassWithInvalidTypes.class));
        new MetamodelImpl(conf, entityLoaderMock);
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
        when(entityLoaderMock.discoverEntityClasses(conf))
                .thenReturn(Collections.singleton(ClassWithInvalidProperties.class));
        new MetamodelImpl(conf, entityLoaderMock);
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithInvalidProperties")
    private static class ClassWithInvalidProperties {
        @Id
        private String id;
        @Properties
        private List<String> properties;
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwExceptionWhenForClassWithInvalidIdentifier() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf))
                .thenReturn(Collections.singleton(ClassWithInvalidIdentifier.class));
        new MetamodelImpl(conf, entityLoaderMock);
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithInvalidIdentifier")
    private static class ClassWithInvalidIdentifier {
        @Id
        private Integer id;
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionForClassWithoutIdentifier() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf))
                .thenReturn(Collections.singleton(ClassWithoutIdentifier.class));
        new MetamodelImpl(conf, entityLoaderMock);
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithoutIdentifier")
    private static class ClassWithoutIdentifier {
        @Properties
        private Map<String, Set<String>> properties;
    }

    @Test
    public void processesClassWhenItIsMissingInTypeMap() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.emptySet());
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);
        final EntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        assertNotNull(et);
    }

    @Test
    public void buildsSingleEntityWithSingularDataPropertyWithNonEmptyField() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassN.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassN> et = metamodel.entity(OWLClassN.class);
        final Field strAttField = OWLClassN.getStringAttributeField();
        final FieldSpecification<? super OWLClassN, ?> att = et.getFieldSpecification(strAttField.getName());
        assertTrue(att instanceof SingularAttribute);
        final SingularAttribute<? super OWLClassN, ?> singularString = (SingularAttribute<? super OWLClassN, ?>) att;
        assertTrue(singularString.isNonEmpty());
    }

    @Test
    public void skipsStaticFieldsWhenProcessingClass() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassO.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassO> et = metamodel.entity(OWLClassO.class);
        assertNull(et.getFieldSpecification("STR_ATT_FIELD"));
        assertNull(et.getFieldSpecification("primitiveField"));
        assertEquals(1, et.getDeclaredAttributes().size());
    }

    @Test
    public void skipsFieldsAnnotatedWithTransientWhenProcessingClass() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassO.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassO> et = metamodel.entity(OWLClassO.class);
        assertNull(et.getFieldSpecification(OWLClassO.TRANSIENT_ANNOTATED_FIELD_NAME));
        assertEquals(1, et.getDeclaredAttributes().size());
    }

    @Test
    public void skipsTransientFieldsWhenProcessingClass() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassO.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassO> et = metamodel.entity(OWLClassO.class);
        assertNull(et.getFieldSpecification(OWLClassO.TRANSIENT_FIELD_NAME));
        assertEquals(1, et.getDeclaredAttributes().size());
    }

    @Test
    public void skipsFinalFieldsWhenProcessingClass() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassO.class));
        final Metamodel metamodel = new MetamodelImpl(conf, entityLoaderMock);

        final EntityType<OWLClassO> et = metamodel.entity(OWLClassO.class);
        assertNull(et.getFieldSpecification(OWLClassO.TRANSIENT_FINAL_FIELD_NAME));
        assertEquals(1, et.getDeclaredAttributes().size());
    }

    @Test(expected = MetamodelInitializationException.class)
    public void throwsExceptionForEntityWithoutNoArgConstructor() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf))
                .thenReturn(Collections.singleton(ClassWithoutNoArgConstructor.class));

        new MetamodelImpl(conf, entityLoaderMock);
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithoutNoArgConstructor")
    private static class ClassWithoutNoArgConstructor {

        @Id
        private URI id;

        public ClassWithoutNoArgConstructor(URI id) {
            this.id = id;
        }
    }
}