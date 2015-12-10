package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AxiomValueGathererTest {

    private static final NamedResource SUBJECT = NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa#Subject");
    private static final Assertion DATA_ASSERTION = Assertion.createDataPropertyAssertion(
            URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#dataProperty"), false);
    private static final Assertion OBJECT_ASSERTION = Assertion.createObjectPropertyAssertion(
            URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#dataProperty"), false);
    private static final URI CONTEXT = URI.create("http://krizik.felk.cvutcz/ontologies/jopa#contextOne");

    @Mock
    private Connection connectionMock;
    @Mock
    private Lists listsMock;
    @Mock
    private Types typesMock;
    @Mock
    private Properties propertiesMock;

    private AxiomValueGatherer gatherer;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectionMock.lists()).thenReturn(listsMock);
        when(connectionMock.types()).thenReturn(typesMock);
        when(connectionMock.properties()).thenReturn(propertiesMock);

        this.gatherer = new AxiomValueGatherer(SUBJECT, null);
    }

    @Test
    public void testAddValue() throws Exception {
        final Value<String> val = addValue();
        final Field axDescField = AxiomValueGatherer.class.getDeclaredField("axiomDescriptor");
        axDescField.setAccessible(true);
        final AxiomValueDescriptor desc = (AxiomValueDescriptor) axDescField.get(gatherer);
        assertNotNull(desc);
        final List<Value<?>> values = desc.getAssertionValues(DATA_ASSERTION);
        assertNotNull(values);
        assertEquals(1, values.size());
        assertTrue(values.contains(val));
        assertEquals(CONTEXT, desc.getAssertionContext(DATA_ASSERTION));
    }

    private Value<String> addValue() {
        final Value<String> val = new Value<>("SomeValue");
        gatherer.addValue(DATA_ASSERTION, val, CONTEXT);
        return val;
    }

    @Test
    public void testAddPropertiesTwice() throws Exception {
        final Map<Assertion, Set<Value<?>>> propsOne = new HashMap<>();
        propsOne.put(DATA_ASSERTION, new HashSet<>(Arrays.asList(new Value<>("one"), new Value<>("two"))));
        gatherer.addProperties(propsOne, null);
        final Map<Assertion, Set<Value<?>>> propsTwo = new HashMap<>();
        propsTwo.put(OBJECT_ASSERTION, Collections.<Value<?>>singleton(new Value<>("http://krizik.felk.cvut.cz/pp")));
        propsTwo.put(DATA_ASSERTION, Collections.<Value<?>>singleton(new Value<>("117")));
        gatherer.addProperties(propsTwo, null);

        final Map<Assertion, Set<Value<?>>> res = OOMTestUtils.getPropertiesToAdd(gatherer);
        verifyResultContainsAll(res, propsOne);
        verifyResultContainsAll(res, propsTwo);
    }

    private void verifyResultContainsAll(Map<Assertion, Set<Value<?>>> res, Map<Assertion, Set<Value<?>>> props) {
        for (Map.Entry<Assertion, Set<Value<?>>> e : props.entrySet()) {
            assertTrue(res.containsKey(e.getKey()));
            assertTrue(res.get(e.getKey()).containsAll(e.getValue()));
        }
    }

    @Test
    public void testPersist() throws Exception {
        addValue();
        final SimpleListValueDescriptor simpleD = mock(SimpleListValueDescriptor.class);
        final ReferencedListValueDescriptor referencedD = mock(ReferencedListValueDescriptor.class);
        gatherer.addSimpleListValues(simpleD);
        gatherer.addReferencedListValues(referencedD);
        final Set<URI> typesToAdd = typesToAdd();
        gatherer.addTypes(typesToAdd, null);
        final Map<Assertion, Set<Value<?>>> propsToAdd = propertiesToAdd();
        gatherer.addProperties(propsToAdd, null);

        gatherer.persist(connectionMock);
        verify(connectionMock).persist(any(AxiomValueDescriptor.class));
        verify(listsMock).persistSimpleList(simpleD);
        verify(listsMock).persistReferencedList(referencedD);
        verify(typesMock).addTypes(SUBJECT, null, typesToAdd);
        verify(propertiesMock).addProperties(SUBJECT, null, propsToAdd);
    }

    private Set<URI> typesToAdd() {
        final Set<URI> set = new HashSet<>();
        set.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeOne"));
        set.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeTwo"));
        set.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeThre"));
        return set;
    }

    private Map<Assertion, Set<Value<?>>> propertiesToAdd() {
        final Map<Assertion, Set<Value<?>>> props = new HashMap<>();
        props.put(DATA_ASSERTION, Collections.<Value<?>>singleton(new Value<>("valueOne")));
        props.put(OBJECT_ASSERTION, Collections.<Value<?>>singleton(
                new Value<>(URI.create("http://krizik.felk.cvut.cz/valueTwo"))));
        return props;
    }

    @Test
    public void testUpdate() throws Exception {
        addValue();
        final SimpleListValueDescriptor simpleD = mock(SimpleListValueDescriptor.class);
        final ReferencedListValueDescriptor referencedD = mock(ReferencedListValueDescriptor.class);
        gatherer.addSimpleListValues(simpleD);
        gatherer.addReferencedListValues(referencedD);
        final Set<URI> typesToAdd = typesToAdd();
        gatherer.addTypes(typesToAdd, null);
        final Set<URI> typesToRemove = typesToRemove();
        gatherer.removeTypes(typesToRemove, null);
        final Map<Assertion, Set<Value<?>>> propsToAdd = propertiesToAdd();
        gatherer.addProperties(propsToAdd, null);
        final Map<Assertion, Set<Value<?>>> propsToRemove = propertiesToRemove();
        gatherer.removeProperties(propsToRemove, null);

        gatherer.update(connectionMock);
        verify(connectionMock).update(any(AxiomValueDescriptor.class));
        verify(listsMock).updateSimpleList(simpleD);
        verify(listsMock).updateReferencedList(referencedD);
        verify(typesMock).addTypes(SUBJECT, null, typesToAdd);
        verify(typesMock).removeTypes(SUBJECT, null, typesToRemove);
        verify(propertiesMock).addProperties(SUBJECT, null, propsToAdd);
        verify(propertiesMock).removeProperties(SUBJECT, null, propsToRemove);
    }

    private Set<URI> typesToRemove() {
        final Set<URI> set = new HashSet<>();
        set.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeFour"));
        set.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeFive"));
        set.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeSix"));
        return set;
    }

    private Map<Assertion, Set<Value<?>>> propertiesToRemove() {
        final Map<Assertion, Set<Value<?>>> props = new HashMap<>();
        props.put(DATA_ASSERTION, Collections.<Value<?>>singleton(new Value<>("valueThree")));
        props.put(DATA_ASSERTION, Collections.<Value<?>>singleton(new Value<>("valueFour")));
        return props;
    }
}