package cz.cvut.kbss.jopa.model.util;

import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.util.Property;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

public class PropertyTest {

    @Test
    void owlPropertiesAreMarkedWithPropertyAnnotation() {
        assertTrue(OWLAnnotationProperty.class.isAnnotationPresent(Property.class));
        assertTrue(OWLDataProperty.class.isAnnotationPresent(Property.class));
        assertTrue(OWLObjectProperty.class.isAnnotationPresent(Property.class));
    }
}
