/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class AxiomValueGathererTest {

    private static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa#Subject");
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

    private AxiomValueGatherer sut;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        when(connectionMock.lists()).thenReturn(listsMock);
        when(connectionMock.types()).thenReturn(typesMock);
        when(connectionMock.properties()).thenReturn(propertiesMock);

        this.sut = new AxiomValueGatherer(SUBJECT, null);
    }

    @Test
    void testAddValue() throws Exception {
        final Value<String> val = addValue();
        final AxiomValueDescriptor desc = getAxiomValueDescriptor();
        final List<Value<?>> values = desc.getAssertionValues(DATA_ASSERTION);
        assertNotNull(values);
        assertEquals(1, values.size());
        assertTrue(values.contains(val));
        assertEquals(CONTEXT, desc.getAssertionContext(DATA_ASSERTION));
    }

    private AxiomValueDescriptor getAxiomValueDescriptor() throws Exception {
        final Field axDescField = AxiomValueGatherer.class.getDeclaredField("axiomDescriptor");
        axDescField.setAccessible(true);
        final AxiomValueDescriptor desc = (AxiomValueDescriptor) axDescField.get(sut);
        assertNotNull(desc);
        return desc;
    }

    private Value<String> addValue() {
        final Value<String> val = new Value<>("SomeValue");
        sut.addValue(DATA_ASSERTION, val, CONTEXT);
        return val;
    }

    @Test
    void testAddPropertiesTwice() throws Exception {
        final Map<Assertion, Set<Value<?>>> propsOne = new HashMap<>();
        propsOne.put(DATA_ASSERTION, new HashSet<>(Arrays.asList(new Value<>("one"), new Value<>("two"))));
        sut.addProperties(propsOne, null);
        final Map<Assertion, Set<Value<?>>> propsTwo = new HashMap<>();
        propsTwo.put(OBJECT_ASSERTION, Collections.singleton(new Value<>("http://krizik.felk.cvut.cz/pp")));
        propsTwo.put(DATA_ASSERTION, Collections.singleton(new Value<>("117")));
        sut.addProperties(propsTwo, null);

        final Map<Assertion, Set<Value<?>>> res = OOMTestUtils.getPropertiesToAdd(sut);
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
    void testPersist() throws Exception {
        addValue();
        final SimpleListValueDescriptor simpleD = mock(SimpleListValueDescriptor.class);
        final ReferencedListValueDescriptor referencedD = mock(ReferencedListValueDescriptor.class);
        sut.addSimpleListValues(simpleD);
        sut.addReferencedListValues(referencedD);
        final Set<URI> typesToAdd = generateTypes();
        sut.addTypes(typesToAdd, null);
        final Map<Assertion, Set<Value<?>>> propsToAdd = propertiesToAdd();
        sut.addProperties(propsToAdd, null);

        sut.persist(connectionMock);
        verify(connectionMock).persist(any(AxiomValueDescriptor.class));
        verify(listsMock).persistSimpleList(simpleD);
        verify(listsMock).persistReferencedList(referencedD);
        verify(typesMock).addTypes(SUBJECT, null, typesToAdd);
        verify(propertiesMock).addProperties(SUBJECT, null, propsToAdd);
    }

    private Set<URI> generateTypes() {
        return IntStream.range(0, 3).mapToObj(i -> Generators.createIndividualIdentifier()).collect(Collectors.toSet());
    }

    private Map<Assertion, Set<Value<?>>> propertiesToAdd() {
        final Map<Assertion, Set<Value<?>>> props = new HashMap<>();
        props.put(DATA_ASSERTION, Collections.singleton(new Value<>("valueOne")));
        props.put(OBJECT_ASSERTION,
                Collections.singleton(new Value<>(URI.create("http://krizik.felk.cvut.cz/valueTwo"))));
        return props;
    }

    @Test
    void testUpdate() throws Exception {
        addValue();
        final SimpleListValueDescriptor simpleD = mock(SimpleListValueDescriptor.class);
        final ReferencedListValueDescriptor referencedD = mock(ReferencedListValueDescriptor.class);
        sut.addSimpleListValues(simpleD);
        sut.addReferencedListValues(referencedD);
        final Set<URI> typesToAdd = generateTypes();
        sut.addTypes(typesToAdd, null);
        final Set<URI> typesToRemove = generateTypes();
        sut.removeTypes(typesToRemove, null);
        final Map<Assertion, Set<Value<?>>> propsToAdd = propertiesToAdd();
        sut.addProperties(propsToAdd, null);
        final Map<Assertion, Set<Value<?>>> propsToRemove = propertiesToRemove();
        sut.removeProperties(propsToRemove, null);

        sut.update(connectionMock);
        verify(connectionMock).update(any(AxiomValueDescriptor.class));
        verify(listsMock).updateSimpleList(simpleD);
        verify(listsMock).updateReferencedList(referencedD);
        verify(typesMock).addTypes(SUBJECT, null, typesToAdd);
        verify(typesMock).removeTypes(SUBJECT, null, typesToRemove);
        verify(propertiesMock).addProperties(SUBJECT, null, propsToAdd);
        verify(propertiesMock).removeProperties(SUBJECT, null, propsToRemove);
    }

    private Map<Assertion, Set<Value<?>>> propertiesToRemove() {
        final Map<Assertion, Set<Value<?>>> props = new HashMap<>();
        props.put(DATA_ASSERTION, Collections.singleton(new Value<>("valueFour")));
        return props;
    }

    @Test
    void addValueSetsAssertionContextWhenItIsDefaultAndSubjectContextIsDifferent() throws Exception {
        this.sut = new AxiomValueGatherer(SUBJECT, CONTEXT);
        final Value<String> value = new Value<>("test value");
        sut.addValue(DATA_ASSERTION, value, null);

        final AxiomValueDescriptor result = getAxiomValueDescriptor();
        assertEquals(CONTEXT, result.getSubjectContext());
        assertTrue(result.containsAssertion(DATA_ASSERTION));
        assertNull(result.getAssertionContext(DATA_ASSERTION));
    }
}
