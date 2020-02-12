/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class MutationAxiomDescriptorTest {

    private Assertion dpAssertion;

    private AxiomValueDescriptor sut;

    @BeforeEach
    void setUp() {
        this.dpAssertion = Assertion.createDataPropertyAssertion(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
                false);
        this.sut = new AxiomValueDescriptor(
                NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));

    }

    @Test
    void testMutationAxiomDescriptorNullArg() {
        assertThrows(NullPointerException.class, () -> this.sut = new AxiomValueDescriptor(null));
    }

    @Test
    void testAddAssertionValueExistingAssertion() {
        final Value<String> val = new Value<>("JustASimpleStringValue");
        sut.addAssertion(dpAssertion);
        assertTrue(sut.getAssertions().contains(dpAssertion));
        sut.addAssertionValue(dpAssertion, val);
        assertFalse(sut.getAssertionValues(dpAssertion).isEmpty());
    }

    @Test
    void testAddAssertionValueForPreviouslyNotExistingAssertion() {
        final Value<String> val = new Value<>("JustASimpleStringValue");
        assertFalse(sut.getAssertions().contains(dpAssertion));
        sut.addAssertionValue(dpAssertion, val);
        assertFalse(sut.getAssertionValues(dpAssertion).isEmpty());
        assertTrue(sut.getAssertions().contains(dpAssertion));
    }

    @Test
    void testAddMultipleAssertionValues() {
        final Value<String> val = new Value<>("JustASimpleStringValue");
        final Value<String> val2 = new Value<>("AnotherStringValue");
        sut.addAssertionValue(dpAssertion, val);
        sut.addAssertionValue(dpAssertion, val2);
        final List<Value<?>> res = sut.getAssertionValues(dpAssertion);
        assertEquals(2, res.size());
        assertTrue(res.contains(val));
        assertTrue(res.contains(val2));
    }

    @Test
    void testGetAssertionValuesEmpty() {
        assertNotNull(sut.getAssertionValues(dpAssertion));
        assertTrue(sut.getAssertionValues(dpAssertion).isEmpty());
        sut.addAssertion(dpAssertion);
        assertTrue(sut.getAssertionValues(dpAssertion).isEmpty());
    }

    @Test
    void testGetAssertionValues() {
        final Value<String> val = new Value<>("JustASimpleStringValue");
        sut.addAssertionValue(dpAssertion, val);
        final List<Value<?>> res = sut.getAssertionValues(dpAssertion);
        assertEquals(1, res.size());
        assertEquals(val, res.get(0));
    }

    @Test
    void testAddAssertion() {
        sut.addAssertion(dpAssertion);
        assertTrue(sut.getAssertions().contains(dpAssertion));
    }

    @Test
    void testAddAssertionNull() {
        assertThrows(NullPointerException.class, () -> sut.addAssertion(null));
    }
}
