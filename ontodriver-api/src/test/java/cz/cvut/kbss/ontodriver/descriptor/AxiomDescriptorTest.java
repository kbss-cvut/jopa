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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;

class AxiomDescriptorTest {

    private static final URI CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
    private static final Assertion ASSERTION = Assertion
            .createPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/propertyOne"), false);

    private AxiomDescriptor sut;

    @BeforeEach
    void setUp() {
        sut = new AxiomDescriptor(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
    }

    @Test
    void testSetSubjectContext() {
        assertNull(sut.getSubjectContext());
        sut.setSubjectContext(CONTEXT);
        assertNotNull(sut.getSubjectContext());
        assertEquals(CONTEXT, sut.getSubjectContext());
    }

    @Test
    void testSetAssertionContext() {
        sut.addAssertion(ASSERTION);
        assertNull(sut.getAssertionContext(ASSERTION));
        sut.setAssertionContext(ASSERTION, CONTEXT);
        assertEquals(CONTEXT, sut.getAssertionContext(ASSERTION));
    }

    @Test
    void testSetAssertionContextInvalid() {
        assertThrows(IllegalArgumentException.class, () -> sut.setAssertionContext(ASSERTION, CONTEXT));
    }
}
