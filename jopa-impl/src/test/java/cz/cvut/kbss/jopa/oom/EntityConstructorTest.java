package cz.cvut.kbss.jopa.oom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.Map.Entry;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassJ;
import cz.cvut.kbss.jopa.test.utils.Generators;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class EntityConstructorTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityX");
    private static final URI PK_TWO = URI
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityXX");
    private static final String STRING_ATT = "StringAttributeValue";
    private static final Set<String> TYPES = initTypes();

    @Mock
    private ObjectOntologyMapperImpl mapperMock;

    @Mock
    private EntityType<OWLClassA> etAMock;
    @Mock
    private Attribute strAttAMock;
    @Mock
    private TypesSpecification typesSpecMock;
    @Mock
    private Identifier idAMock;

    @Mock
    private EntityType<OWLClassB> etBMock;
    @Mock
    private Attribute strAttBMock;
    @Mock
    private PropertiesSpecification propsSpecMock;
    @Mock
    private Identifier idBMock;

    @Mock
    private EntityType<OWLClassD> etDMock;
    @Mock
    private Attribute clsAAttMock;
    @Mock
    private Identifier idDMock;

    @Mock
    private EntityType<OWLClassJ> etJMock;
    @Mock
    private PluralAttribute aSetAttMock;
    @Mock
    private Identifier idJMock;

    private Descriptor descriptor;

    private EntityConstructor constructor;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        TestEnvironmentUtils.initOWLClassAMocks(etAMock, strAttAMock, typesSpecMock);
        when(etAMock.getIdentifier()).thenReturn(idAMock);
        when(idAMock.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
        TestEnvironmentUtils.initOWLClassBMocks(etBMock, strAttBMock, propsSpecMock);
        when(etBMock.getIdentifier()).thenReturn(idBMock);
        when(idBMock.getJavaField()).thenReturn(OWLClassB.class.getDeclaredField("uri"));
        TestEnvironmentUtils.initOWLClassDMocks(etDMock, clsAAttMock);
        when(etDMock.getIdentifier()).thenReturn(idDMock);
        when(idDMock.getJavaField()).thenReturn(OWLClassD.class.getDeclaredField("uri"));
        TestEnvironmentUtils.initOWLClassJMocks(etJMock, aSetAttMock, idJMock);
        this.descriptor = new EntityDescriptor();
        this.constructor = new EntityConstructor(mapperMock);
    }

    @Test
    public void testReconstructEntityWithTypesAndDataProperty() throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(OWLClassA.getClassIri()));
        axioms.add(getStringAttAssertionAxiom(OWLClassA.getStrAttField()));
        axioms.addAll(getTypesAxiomsForOwlClassA());
        final OWLClassA res = constructor.reconstructEntity(PK, etAMock, descriptor, axioms);
        assertNotNull(res);
        assertEquals(PK, res.getUri());
        assertEquals(STRING_ATT, res.getStringAttribute());
        assertEquals(TYPES, res.getTypes());
        verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
    }

    private Axiom<URI> getClassAssertionAxiomForType(String type) {
        return new AxiomImpl<>(NamedResource.create(PK), Assertion.createClassAssertion(false), new Value<>(URI.create(type)));
    }

    private Axiom<String> getStringAttAssertionAxiom(Field attField) throws Exception {
        final Axiom<String> ax = mock(Axiom.class);
        when(ax.getSubject()).thenReturn(NamedResource.create(PK));
        final String assertionIri = attField.getAnnotation(OWLDataProperty.class).iri();
        when(ax.getAssertion()).thenReturn(
                Assertion.createDataPropertyAssertion(URI.create(assertionIri), false));
        when(ax.getValue()).thenReturn(new Value<String>(STRING_ATT));
        return ax;
    }

    private Set<Axiom<?>> getTypesAxiomsForOwlClassA() throws Exception {
        final Set<Axiom<?>> axs = new HashSet<>();
        for (String type : TYPES) {
            final Axiom<URI> ax = new AxiomImpl<>(NamedResource.create(PK), Assertion.createClassAssertion(false), new Value<>(URI.create(type)));
            axs.add(ax);
        }
        return axs;
    }

    @Test
    public void testReconstructEntityWithDataPropertyEmptyTypes() throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(OWLClassA.getClassIri()));
        axioms.add(getStringAttAssertionAxiom(OWLClassA.getStrAttField()));
        final OWLClassA res = constructor.reconstructEntity(PK, etAMock, descriptor, axioms);
        assertNotNull(res);
        assertEquals(PK, res.getUri());
        assertEquals(STRING_ATT, res.getStringAttribute());
        assertNull(res.getTypes());
        verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
    }

    @Test
    public void testReconstructEntityWithDataPropertyAndProperties() throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(OWLClassB.getClassIri()));
        axioms.add(getStringAttAssertionAxiom(OWLClassB.getStrAttField()));
        final Collection<Axiom<?>> properties = getProperties();
        axioms.addAll(properties);
        final OWLClassB res = constructor.reconstructEntity(PK, etBMock, descriptor, axioms);
        assertNotNull(res);
        assertEquals(PK, res.getUri());
        assertEquals(STRING_ATT, res.getStringAttribute());
        assertNotNull(res.getProperties());
        assertEquals(properties.size(), res.getProperties().size());
        for (Axiom<?> a : properties) {
            final String key = a.getAssertion().getIdentifier().toString();
            assertTrue(res.getProperties().containsKey(key));
            assertEquals(1, res.getProperties().get(key).size());
            assertEquals(a.getValue().stringValue(), res.getProperties().get(key).iterator().next());
        }
        verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
    }

    private Collection<Axiom<?>> getProperties() {
        final Set<Axiom<?>> props = new HashSet<>();
        final Axiom<String> axOne = mock(Axiom.class);
        when(axOne.getSubject()).thenReturn(NamedResource.create(PK));
        when(axOne.getAssertion()).thenReturn(
                Assertion.createDataPropertyAssertion(URI.create("http://someDataPropertyOne"),
                        false));
        when(axOne.getValue()).thenReturn(new Value<String>("SomePropertyValue"));
        props.add(axOne);

        final Axiom<String> axTwo = mock(Axiom.class);
        when(axTwo.getSubject()).thenReturn(NamedResource.create(PK));
        when(axTwo.getAssertion()).thenReturn(
                Assertion.createAnnotationPropertyAssertion(
                        URI.create("http://someAnnotationPropertyOne"), false));
        when(axTwo.getValue()).thenReturn(new Value<String>("annotationValue"));
        props.add(axTwo);

        final Axiom<URI> axThree = mock(Axiom.class);
        when(axThree.getSubject()).thenReturn(NamedResource.create(PK));
        when(axThree.getAssertion()).thenReturn(
                Assertion.createObjectPropertyAssertion(URI.create("http://someObjectPropertyOne"),
                        false));
        when(axThree.getValue())
                .thenReturn(
                        new Value<URI>(URI
                                .create("http://krizik.felk.cvut.cz/ontologies/jopa/otherEntity")));
        props.add(axThree);
        return props;
    }

    @Test
    public void testReconstructEntityWithDataPropertiesAndEmptyProperties() throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(OWLClassB.getClassIri()));
        axioms.add(getStringAttAssertionAxiom(OWLClassB.getStrAttField()));
        final OWLClassB res = constructor.reconstructEntity(PK, etBMock, descriptor, axioms);
        assertNotNull(res);
        assertEquals(PK, res.getUri());
        assertEquals(STRING_ATT, res.getStringAttribute());
        assertNull(res.getProperties());
        verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
    }

    @Test
    public void testReconstructEntityWithObjectProperty() throws Exception {
        final Set<Axiom<?>> axiomsD = getAxiomsForD();
        final Descriptor fieldDesc = mock(Descriptor.class);
        descriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), fieldDesc);
        final OWLClassA entityA = new OWLClassA();
        entityA.setUri(PK_TWO);
        entityA.setStringAttribute(STRING_ATT);
        when(clsAAttMock.getFetchType()).thenReturn(FetchType.EAGER);
        when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, PK_TWO, fieldDesc))
                .thenReturn(entityA);
        final OWLClassD res = constructor.reconstructEntity(PK, etDMock, descriptor, axiomsD);
        assertNotNull(res);
        assertEquals(PK, res.getUri());
        verify(mapperMock).getEntityFromCacheOrOntology(OWLClassA.class, PK_TWO, fieldDesc);
        assertNotNull(res.getOwlClassA());
        assertEquals(PK_TWO, res.getOwlClassA().getUri());
        assertEquals(STRING_ATT, res.getOwlClassA().getStringAttribute());
        verify(mapperMock).registerInstance(PK, res, descriptor.getContext());
    }

    private Set<Axiom<?>> getAxiomsForD() throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(OWLClassD.getClassIri()));
        final Axiom<NamedResource> opAssertion = createObjectPropertyAxiomForD();
        axioms.add(opAssertion);
        return axioms;
    }

    private Axiom<NamedResource> createObjectPropertyAxiomForD() throws NoSuchFieldException {
        final URI assertionUri = URI.create(OWLClassD.getOwlClassAField()
                .getAnnotation(OWLObjectProperty.class).iri());
        final Assertion assertion = Assertion.createObjectPropertyAssertion(assertionUri, false);
        final Axiom<NamedResource> opAssertion =
                new AxiomImpl<>(NamedResource.create(PK), assertion, new Value<>(NamedResource.create(PK_TWO)));
        return opAssertion;
    }

    @Test
    public void testSetFieldValue_DataProperty() throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getStringAttAssertionAxiom(OWLClassA.getStrAttField()));
        final OWLClassA entityA = new OWLClassA();
        entityA.setUri(PK);
        assertNull(entityA.getStringAttribute());
        constructor.setFieldValue(entityA, OWLClassA.getStrAttField(), axioms, etAMock, descriptor);
        assertNotNull(entityA.getStringAttribute());
        assertEquals(STRING_ATT, entityA.getStringAttribute());
    }

    @Test
    public void testSetFieldValue_ObjectProperty() throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<>();
        final Descriptor fieldDesc = mock(Descriptor.class);
        descriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), fieldDesc);
        axioms.add(createObjectPropertyAxiomForD());
        final OWLClassD entityD = new OWLClassD();
        entityD.setUri(PK);
        assertNull(entityD.getOwlClassA());
        final OWLClassA entityA = new OWLClassA();
        entityA.setUri(PK_TWO);
        entityA.setStringAttribute(STRING_ATT);
        when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, PK_TWO, fieldDesc))
                .thenReturn(entityA);
        constructor.setFieldValue(entityD, OWLClassD.getOwlClassAField(), axioms, etDMock,
                descriptor);
        assertNotNull(entityD.getOwlClassA());
        assertEquals(PK_TWO, entityD.getOwlClassA().getUri());
        verify(mapperMock).getEntityFromCacheOrOntology(OWLClassA.class, PK_TWO, fieldDesc);
    }

    @Test
    public void testSetFieldValue_Types() throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<Axiom<?>>(getTypesAxiomsForOwlClassA());
        final Descriptor fieldDesc = mock(Descriptor.class);
        descriptor.addAttributeDescriptor(OWLClassA.getTypesField(), fieldDesc);
        final OWLClassA entityA = new OWLClassA();
        entityA.setUri(PK);
        assertNull(entityA.getTypes());
        constructor.setFieldValue(entityA, OWLClassA.getTypesField(), axioms, etAMock, descriptor);
        assertNotNull(entityA.getTypes());
        assertEquals(TYPES.size(), entityA.getTypes().size());
        assertTrue(entityA.getTypes().containsAll(TYPES));
    }

    @Test
    public void testSetFieldValue_Properties() throws Exception {
        final Map<String, Set<String>> props = Generators.createProperties();
        final Set<Axiom<?>> axioms = initAxiomsForProperties(props);
        final OWLClassB entityB = new OWLClassB();
        entityB.setUri(PK);
        assertNull(entityB.getProperties());
        constructor.setFieldValue(entityB, OWLClassB.getPropertiesField(), axioms, etBMock,
                descriptor);
        assertNotNull(entityB.getProperties());
        assertEquals(props, entityB.getProperties());
    }

    private Set<Axiom<?>> initAxiomsForProperties(Map<String, Set<String>> props) {
        final Set<Axiom<?>> axioms = new HashSet<>();
        for (Entry<String, Set<String>> e : props.entrySet()) {
            final URI property = URI.create(e.getKey());
            for (String val : e.getValue()) {
                axioms.add(new AxiomImpl<URI>(NamedResource.create(PK), Assertion
                        .createPropertyAssertion(property, false), new Value<URI>(URI.create(val))));
            }
        }
        return axioms;
    }

    @Test
    public void testSetFieldValue_ObjectPropertySet() throws Exception {
        final Descriptor desc = mock(Descriptor.class);
        final Set<OWLClassA> set = initEntities(desc);
        descriptor.addAttributeDescriptor(OWLClassJ.getOwlClassAField(), desc);
        final Collection<Axiom<?>> axioms = initAxiomsForReferencedSet(set);
        final OWLClassJ entityJ = new OWLClassJ();
        entityJ.setUri(PK);
        assertNull(entityJ.getOwlClassA());
        constructor.setFieldValue(entityJ, OWLClassJ.getOwlClassAField(), axioms, etJMock,
                descriptor);
        assertNotNull(entityJ.getOwlClassA());
        assertEquals(set.size(), entityJ.getOwlClassA().size());
        assertTrue(areEqual(set, entityJ.getOwlClassA()));
    }

    private Set<OWLClassA> initEntities(Descriptor desc) {
        final Set<OWLClassA> ents = new HashSet<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://krizik.felk.cvut.cz/entityA" + i));
            a.setStringAttribute(STRING_ATT);
            ents.add(a);
            when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, a.getUri(), desc))
                    .thenReturn(a);
        }
        return ents;
    }

    private Collection<Axiom<?>> initAxiomsForReferencedSet(Set<OWLClassA> as) throws Exception {
        final Set<Axiom<?>> axioms = new HashSet<>();
        final URI property = URI.create(OWLClassJ.getOwlClassAField()
                .getAnnotation(OWLObjectProperty.class).iri());
        for (OWLClassA a : as) {
            final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(PK),
                    Assertion.createObjectPropertyAssertion(property, false), new Value<NamedResource>(NamedResource.create(
                    a.getUri())));
            axioms.add(ax);
        }
        return axioms;
    }

    private boolean areEqual(Set<OWLClassA> expected, Set<OWLClassA> actual) {
        boolean found = false;
        for (OWLClassA a : expected) {
            found = false;
            for (OWLClassA aa : actual) {
                if (a.getUri().equals(aa.getUri())) {
                    found = true;
                    assertTrue(a.getStringAttribute().equals(aa.getStringAttribute()));
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return found;
    }

    private static Set<String> initTypes() {
        final Set<String> set = new HashSet<>(8);
        set.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
        set.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassV");
        return set;
    }
}
