package cz.cvut.kbss.jopa.model;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class IRITest {

    @Test
    void twoInstancesAreEqualWhenTheirValueEqual() {
        final String iri = "http://onto.fel.cvut.cz";
        final IRI one = IRI.create(iri);
        final IRI two = IRI.create(iri);
        assertEquals(one, two);
        assertEquals(one.hashCode(), two.hashCode());
        final IRI different = IRI.create("http://kbss.felk.cvut.cz");
        assertNotEquals(one, different);
    }
}
