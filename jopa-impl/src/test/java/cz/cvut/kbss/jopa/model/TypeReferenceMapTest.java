package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Set;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class TypeReferenceMapTest {

    private final TypeReferenceMap sut = new TypeReferenceMap();

    @Test
    void getReferringTypesReturnsEmptySetForUnknownClass() {
        final Set<Class<?>> result = sut.getReferringTypes(String.class);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void addReferenceWorksFirstTimeForType() {
        sut.addReference(OWLClassA.class, OWLClassD.class);
        final Set<Class<?>> result = sut.getReferringTypes(OWLClassA.class);
        assertEquals(Collections.singleton(OWLClassD.class), result);
    }

    @Test
    void addReferenceWorksMultipleTimesForType() {
        sut.addReference(OWLClassA.class, OWLClassD.class);
        sut.addReference(OWLClassA.class, OWLClassC.class);
        final Set<Class<?>> result = sut.getReferringTypes(OWLClassA.class);
        assertEquals(2, result.size());
        assertThat(result, hasItems(OWLClassC.class, OWLClassD.class));
    }
}