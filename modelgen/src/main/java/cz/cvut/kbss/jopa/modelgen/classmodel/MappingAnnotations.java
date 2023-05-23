package cz.cvut.kbss.jopa.modelgen.classmodel;

import java.util.Arrays;
import java.util.List;

/**
 * JOPA mapping annotations.
 */
public enum MappingAnnotations {
    ID("cz.cvut.kbss.jopa.model.annotations.Id"),
    PROPERTIES("cz.cvut.kbss.jopa.model.annotations.Properties"),
    OBJECT_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty"),
    DATA_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLDataProperty"),
    ANNOTATION_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty"),
    TYPES("cz.cvut.kbss.jopa.model.annotations.Types");

    private final String annotation;

    MappingAnnotations(String annotation) {
        this.annotation = annotation;
    }

    public String getAnnotation() {
        return annotation;
    }

    public static List<MappingAnnotations> getAll() {
        return Arrays.asList(PROPERTIES, OBJECT_PROPERTY, DATA_PROPERTY, ANNOTATION_PROPERTY, TYPES, ID);
    }

}
