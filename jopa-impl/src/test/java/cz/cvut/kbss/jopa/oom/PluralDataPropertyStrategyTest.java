package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class PluralDataPropertyStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();
    private static final NamedResource INDIVIDUAL = NamedResource.create(PK);


    @Mock
    private EntityMappingHelper mapperMock;

    private AxiomValueGatherer gatherer;

    private MetamodelMocks mocks;
    private Descriptor descriptor = new EntityDescriptor();

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        this.gatherer = new AxiomValueGatherer(INDIVIDUAL, null);
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassM.class)).thenReturn(mocks.forOwlClassM().entityType());
    }

    @Test
    public void buildFieldValueCreatesCorrectCollectionTypeForSet() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        strategy.addValueFromAxiom(createMSetAxiom());
        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertNotNull(m.getIntegerSet());
        assertTrue(m.getIntegerSet() instanceof Set);
    }

    private PluralDataPropertyStrategy<OWLClassM> createStrategyForM() {
        return new PluralDataPropertyStrategy<>(mocks.forOwlClassM().entityType(),
                mocks.forOwlClassM().integerSetAttribute(), descriptor, mapperMock);
    }

    private Axiom<Integer> createMSetAxiom() {
        return new AxiomImpl<>(INDIVIDUAL,
                assertionForMIntegerSet(),
                new Value<>(Generators.randomInt()));
    }

    private Assertion assertionForMIntegerSet() {
        return Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_m_IntegerSet), false);
    }

    @Test
    public void addValueFromAxiomAddsAllValuesToTheCollection() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final Set<Integer> values = addValuesFromAxioms(strategy);

        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertEquals(values, m.getIntegerSet());
    }

    private Set<Integer> addValuesFromAxioms(PluralDataPropertyStrategy<OWLClassM> strategy) {
        final Set<Integer> values = new HashSet<>();
        for (int i = 0; i < Generators.randomPositiveInt(10); i++) {
            final Axiom<Integer> axiom = createMSetAxiom();
            values.add(axiom.getValue().getValue());
            strategy.addValueFromAxiom(axiom);
        }
        return values;
    }

    @Test
    public void addValueFromAxiomSkipsValuesWithInvalidRange() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final Set<Integer> values = addValuesFromAxioms(strategy);
        final Assertion a = assertionForMIntegerSet();
        strategy.addValueFromAxiom(new AxiomImpl<>(INDIVIDUAL, a, new Value<>("Test")));
        strategy.addValueFromAxiom(
                new AxiomImpl<>(INDIVIDUAL, a, new Value<>(Generators.createIndividualIdentifier())));
        strategy.addValueFromAxiom(new AxiomImpl<>(INDIVIDUAL, a, new Value<>(new Date())));

        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertEquals(values, m.getIntegerSet());
    }

    @Test
    public void buildFieldValueDoesNothingWhenNoValuesWereAdded() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        strategy.buildInstanceFieldValue(m);
        assertNull(m.getIntegerSet());
    }

    @Test
    public void buildAxiomValuesFromInstanceAddsAllValuesIntoAxiomValueGatherer() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        m.initializeTestValues(false);
        strategy.buildAxiomValuesFromInstance(m, gatherer);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        assertTrue(valueDescriptor.getAssertions().contains(assertionForMIntegerSet()));
        final List<Value<?>> values = valueDescriptor.getAssertionValues(assertionForMIntegerSet());
        assertEquals(m.getIntegerSet().size(), values.size());
        values.forEach(v -> assertTrue(m.getIntegerSet().contains((Integer) v.getValue())));
    }

    @Test
    public void buildAxiomValuesFromInstancesAddsNullValueWhenAttributeValueIsNull() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();

        strategy.buildAxiomValuesFromInstance(m, gatherer);

        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        assertTrue(valueDescriptor.getAssertions().contains(assertionForMIntegerSet()));
        final List<Value<?>> values = valueDescriptor.getAssertionValues(assertionForMIntegerSet());
        assertEquals(1, values.size());
        assertTrue(values.contains(Value.nullValue()));
    }

    @Test
    public void buildAxiomValuesFromInstanceAddsNullValueWhenAttributeValueIsEmptyCollection() throws Exception {
        final PluralDataPropertyStrategy<OWLClassM> strategy = createStrategyForM();
        final OWLClassM m = new OWLClassM();
        m.setIntegerSet(Collections.emptySet());

        strategy.buildAxiomValuesFromInstance(m, gatherer);

        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        assertTrue(valueDescriptor.getAssertions().contains(assertionForMIntegerSet()));
        final List<Value<?>> values = valueDescriptor.getAssertionValues(assertionForMIntegerSet());
        assertEquals(1, values.size());
        assertTrue(values.contains(Value.nullValue()));
    }
}
