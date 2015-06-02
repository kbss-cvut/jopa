package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class EntityDeconstructorTest {

    private static final URI CONTEXT = URI
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");

    private static OWLClassA entityA;
    private static URI strAttAIdentifier;
    private static OWLClassB entityB;
    private static OWLClassE entityE;
    private static OWLClassD entityD;
    private static URI owlClassAAttIdentifier;
    private static OWLClassK entityK;
    private static OWLClassM entityM;

    @Mock
    private EntityType<OWLClassA> etAMock;
    @Mock
    private Attribute strAttAMock;
    @Mock
    private TypesSpecification typesMock;
    @Mock
    private Identifier idA;

    @Mock
    private EntityType<OWLClassB> etBMock;
    @Mock
    private Attribute strAttBMock;
    @Mock
    private PropertiesSpecification propsMock;
    @Mock
    private Identifier idB;

    @Mock
    private EntityType<OWLClassE> etEMock;
    @Mock
    private Attribute strAttEMock;
    @Mock
    private Identifier idE;

    @Mock
    private EntityType<OWLClassD> etDMock;
    @Mock
    private Attribute clsAMock;
    @Mock
    private Identifier idD;

    @Mock
    private EntityType<OWLClassK> etKMock;
    @Mock
    private Attribute clsEMock;
    @Mock
    private Identifier idK;

    @Mock
    private EntityType<OWLClassM> etMMock;
    @Mock
    private Identifier idM;
    @Mock
    private SingularAttribute booleanAttMock;
    @Mock
    private SingularAttribute intAttMock;
    @Mock
    private SingularAttribute longAttMock;
    @Mock
    private SingularAttribute doubleAttMock;
    @Mock
    private SingularAttribute dateAttMock;

    @Mock
    private ObjectOntologyMapperImpl oomMock;

    @Mock
    private CascadeResolver cascadeResolverMock;

    private EntityDeconstructor entityBreaker;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        entityA = new OWLClassA();
        entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityA"));
        entityA.setStringAttribute("someStringAttribute");
        strAttAIdentifier = URI.create(OWLClassA.getStrAttField()
                                                .getAnnotation(OWLDataProperty.class).iri());
        entityB = new OWLClassB();
        entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityB"));
        entityB.setStringAttribute("entityBStringAttribute");
        entityE = new OWLClassE();
        entityE.setStringAttribute("entityEStringAttribute");
        entityD = new OWLClassD();
        entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityD"));
        entityD.setOwlClassA(entityA);
        owlClassAAttIdentifier = URI.create(OWLClassD.getOwlClassAField()
                                                     .getAnnotation(OWLObjectProperty.class).iri());
        entityK = new OWLClassK();
        entityK.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/entityD"));
        entityK.setOwlClassE(entityE);
        entityM = new OWLClassM();
        entityM.initializeTestValues(true);

    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        TestEnvironmentUtils.initOWLClassAMocks(etAMock, strAttAMock, typesMock, idA);
        TestEnvironmentUtils.initOWLClassBMocks(etBMock, strAttBMock, propsMock, idB);
        TestEnvironmentUtils.initOWLClassEMocks(etEMock, strAttEMock, idE);
        TestEnvironmentUtils.initOWLClassDMocks(etDMock, clsAMock, idD);
        TestEnvironmentUtils.initOWLClassKMocks(etKMock, clsEMock, idK);
        TestEnvironmentUtils.initOWLClassMMock(etMMock, booleanAttMock, intAttMock, longAttMock, doubleAttMock, dateAttMock, idM);
        when(oomMock.getEntityType(OWLClassA.class)).thenReturn(etAMock);
        entityA.setTypes(null);
        entityA.setStringAttribute("someStringAttribute");
        entityB.setProperties(null);
        entityE.setUri(null);
        this.entityBreaker = new EntityDeconstructor(oomMock);
        entityBreaker.setCascadeResolver(cascadeResolverMock);
    }

    @Test
    public void testMapEntityWithDataProperty() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityA.getUri(),
                entityA, etAMock, aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the data property assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier, false)));
    }

    @Test
    public void testMapEntityWithDataPropertyNullValue() throws Exception {
        final OWLClassA entity = new OWLClassA();
        entity.setUri(entityA.getUri());
        final Descriptor aDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entity.getUri(), entity,
                etAMock, aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entity.getUri(), res.getSubject().getIdentifier());
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        final List<Value<?>> v = res.getAssertionValues(Assertion.createDataPropertyAssertion(
                strAttAIdentifier, false));
        assertEquals(1, v.size());
        assertEquals(Value.nullValue(), v.get(0));
    }

    @Test
    public void testMapEntityWithDataPropertyAndTypes() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        final Set<String> types = createTypes();
        entityA.setTypes(types);
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityA.getUri(),
                entityA, etAMock, aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the data property assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier, false)));
        // The entity class + the declared types
        res.getAssertions().stream().filter(a -> a.getType() == AssertionType.CLASS).forEach(a -> {
            final List<Value<?>> cls = res.getAssertionValues(a);
            // The entity class + the declared types
            assertEquals(1, cls.size());
            assertEquals(OWLClassA.getClassIri(), cls.get(0).stringValue());
        });
        final Set<URI> typesRes = OOMTestUtils.getTypesToAdd(builder);
        assertEquals(types.size(), typesRes.size());
        for (URI u : typesRes) {
            assertTrue(types.contains(u.toString()));
        }
    }

    private Set<String> createTypes() {
        final Set<String> types = new HashSet<>();
        types.add("http://krizik.felk.cvut.cz/ontologies/entityX");
        types.add("http://krizik.felk.cvut.cz/ontologies/entityY");
        return types;
    }

    @Test
    public void testMapEntityWithDataPropertyAndTypesPropertyInDifferentContext() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        aDescriptor.addAttributeContext(OWLClassA.getStrAttField(), CONTEXT);
        final Set<String> types = createTypes();
        entityA.setTypes(types);
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityA.getUri(),
                entityA, etAMock, aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getSubject().getIdentifier());
        assertNull(res.getSubjectContext());
        assertEquals(CONTEXT, res.getAssertionContext(Assertion.createDataPropertyAssertion(
                strAttAIdentifier, false)));
        assertNull(res.getAssertionContext(Assertion.createClassAssertion(false)));
    }

    @Test
    public void testMapEntityWithObjectProperty() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityD.getUri(),
                entityD, etDMock, dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
        // Class assertion and the object property assertion
        assertEquals(2, res.getAssertions().size());
        for (Assertion a : res.getAssertions()) {
            final List<Value<?>> vals = res.getAssertionValues(a);
            if (a.getType() == AssertionType.CLASS) {
                assertEquals(1, vals.size());
                assertEquals(URI.create(OWLClassD.getClassIri()), vals.get(0).getValue());
            } else {
                assertTrue(AssertionType.OBJECT_PROPERTY == a.getType());
                assertEquals(1, vals.size());
                assertEquals(NamedResource.create(entityA.getUri()), vals.get(0).getValue());
            }
        }
    }

    @Test
    public void testMapEntityWithObjectPropertyNullValue() throws Exception {
        final OWLClassD entity = new OWLClassD();
        entity.setUri(entityD.getUri());
        final Descriptor dDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entity.getUri(), entity,
                etDMock, dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entity.getUri(), res.getSubject().getIdentifier());
        // Only the class assertion
        assertEquals(2, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        final List<Value<?>> v = res.getAssertionValues(Assertion.createObjectPropertyAssertion(
                owlClassAAttIdentifier, false));
        assertEquals(1, v.size());
        assertEquals(Value.nullValue(), v.get(0));
    }

    @Test
    public void testMapEntityWithObjectPropertyAndContext() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT);
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityD.getUri(),
                entityD, etDMock, dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        assertEquals(entityD.getUri(), res.getSubject().getIdentifier());
        assertEquals(CONTEXT, res.getSubjectContext());
        for (Assertion ass : res.getAssertions()) {
            assertEquals(CONTEXT, res.getAssertionContext(ass));
        }
    }

    @Test
    public void testMapEntityWithProperties() throws Exception {
        final Descriptor bDescriptor = new EntityDescriptor();
        final Map<String, Set<String>> props = Generators.createProperties();
        entityB.setProperties(props);
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
                entityB, etBMock, bDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
        // Class assertion, data property assertion and the properties
        // assertions
        assertEquals(2, res.getAssertions().size());
        verifyPropertiesForSave(props, builder);
    }

    private void verifyPropertiesForSave(Map<String, Set<String>> props, AxiomValueGatherer builder) throws Exception {
        final Map<Assertion, Set<Value<?>>> resultProperties = OOMTestUtils.getPropertiesToAdd(builder);
        for (Assertion a : resultProperties.keySet()) {
            assertTrue(props.containsKey(a.getIdentifier().toString()));
            final Set<String> propValues = props.get(a.getIdentifier().toString());
            for (Value<?> v : resultProperties.get(a)) {
                assertTrue(propValues.contains(v.stringValue()));
            }
        }
    }

    @Test
    public void testMapEntityWithNullProperties() throws Exception {
        final Descriptor bDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
                entityB, etBMock, bDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
        // Class assertion, data property assertion
        assertEquals(2, res.getAssertions().size());
    }

    @Test
    public void testMapEntityWithPropertiesMultipleValuesPerProperty() throws Exception {
        final Map<String, Set<String>> props = createProperties();
        entityB.setProperties(props);
        final Descriptor bDescriptor = new EntityDescriptor();
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityB.getUri(),
                entityB, etBMock, bDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(entityB.getUri(), res.getSubject().getIdentifier());
        assertEquals(2, res.getAssertions().size());
        verifyPropertiesForSave(props, builder);
    }

    private Map<String, Set<String>> createProperties() {
        final Map<String, Set<String>> map = new HashMap<>();
        for (int i = 0; i < 5; i++) {
            final String key = "http://krizik.felk.cvut.cz/ontologies/jopa/someProperty" + i;
            final Set<String> vals = new HashSet<>();
            for (int j = 0; j < 5; j++) {
                vals.add("dataValue" + j);
            }
            map.put(key, vals);
        }
        return map;
    }

    @Test
    public void testMapEntityWithObjectPropertyWithGeneratedIdentifier() throws Exception {
        final URI eUri = URI.create("http://eUri");
        when(oomMock.generateIdentifier(etEMock)).thenReturn(eUri);
        when(oomMock.getEntityType(OWLClassE.class)).thenReturn(etEMock);
        final Descriptor eDescriptor = new EntityDescriptor(CONTEXT);
        final Descriptor kDescriptor = new EntityDescriptor();
        kDescriptor.addAttributeDescriptor(OWLClassK.getOwlClassEField(), eDescriptor);
        assertNull(entityE.getUri());
        final AxiomValueGatherer builder = entityBreaker.mapEntityToAxioms(entityK.getUri(),
                entityK, etKMock, kDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertNotNull(res);
        verify(oomMock).generateIdentifier(etEMock);
        assertEquals(eUri, entityE.getUri());
    }

    @Test
    public void mapsEntityDataPropertyFieldToAxiomDescriptor() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        when(etAMock.getFieldSpecification(OWLClassA.getStrAttField().getName())).thenReturn(
                strAttAMock);
        final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityA.getUri(), entityA,
                OWLClassA.getStrAttField(), etAMock, aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier, strAttAMock.isInferred())));
        final List<Value<?>> val = res.getAssertionValues(res.getAssertions().iterator().next());
        assertEquals(1, val.size());
        assertEquals(entityA.getStringAttribute(), val.get(0).getValue());
    }

    @Test
    public void mapsEntityDataPropertyWithNullValueToAxiomDescriptor() throws Exception {
        final Descriptor aDescriptor = new EntityDescriptor();
        entityA.setStringAttribute(null);
        when(etAMock.getFieldSpecification(OWLClassA.getStrAttField().getName())).thenReturn(
                strAttAMock);
        final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityA.getUri(), entityA,
                OWLClassA.getStrAttField(), etAMock, aDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(strAttAIdentifier, strAttAMock.isInferred())));
        final List<Value<?>> val = res.getAssertionValues(res.getAssertions().iterator().next());
        assertEquals(1, val.size());
        assertEquals(Value.nullValue(), val.get(0));
        assertNull(val.get(0).getValue());
    }

    @Test
    public void mapsEntityObjectPropertyValueInContextToAxiomDescriptor() throws Exception {
        final Descriptor dDescriptor = new EntityDescriptor();
        dDescriptor.addAttributeContext(OWLClassD.getOwlClassAField(), CONTEXT);
        when(etDMock.getFieldSpecification(OWLClassD.getOwlClassAField().getName())).thenReturn(
                clsAMock);
        final AxiomValueGatherer builder = entityBreaker.mapFieldToAxioms(entityD.getUri(), entityD,
                OWLClassD.getOwlClassAField(), etDMock, dDescriptor);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createObjectPropertyAssertion(owlClassAAttIdentifier,
                        strAttAMock.isInferred())));
        final Assertion ass = res.getAssertions().iterator().next();
        final List<Value<?>> val = res.getAssertionValues(ass);
        assertEquals(1, val.size());
        assertEquals(CONTEXT, res.getAssertionContext(ass));
        assertEquals(NamedResource.create(entityA.getUri()), val.get(0).getValue());
    }

    @Test
    public void mapsEntityWithStringKeyAndBasicDataAttributes() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        final AxiomValueGatherer builder =
                entityBreaker.mapEntityToAxioms(URI.create(entityM.getKey()), entityM, etMMock, desc);
        final AxiomValueDescriptor res = getAxiomValueDescriptor(builder);
        assertTrue(containsInstanceAssertion(res));
        assertTrue(containsDPAssertion(res, OWLClassM.getBooleanAttributeField(), entityM.getBooleanAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getIntAttributeField(), entityM.getIntAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getDoubleAttributeField(), entityM.getDoubleAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getLongAttributeField(), entityM.getLongAttribute()));
        assertTrue(containsDPAssertion(res, OWLClassM.getDateAttributeField(), entityM.getDoubleAttribute()));
    }

    private boolean containsInstanceAssertion(AxiomValueDescriptor descriptor) throws Exception {
        final List<Value<?>> values = descriptor.getAssertionValues(Assertion.createClassAssertion(false));
        assertEquals(1, values.size());
        return values.get(0).getValue().toString().equals(OWLClassM.getClassIri());
    }

    private boolean containsDPAssertion(AxiomValueDescriptor descriptor, Field attributeField, Object value) {
        OWLDataProperty annotation = attributeField.getAnnotation(OWLDataProperty.class);
        final URI propertyUri = URI.create(annotation.iri());
        final Assertion assertion = Assertion.createDataPropertyAssertion(propertyUri, attributeField.getAnnotation(
                Inferred.class) != null);
        final List<Value<?>> values = descriptor.getAssertionValues(assertion);
        for (Value<?> val : values) {
            if (val.getValue().equals(value)) {
                return true;
            }
        }
        return false;
    }

    private static AxiomValueDescriptor getAxiomValueDescriptor(AxiomValueGatherer builder)
            throws Exception {
        return OOMTestUtils.getAxiomValueDescriptor(builder);
    }
}
