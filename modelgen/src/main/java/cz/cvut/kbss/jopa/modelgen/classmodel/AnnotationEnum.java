package cz.cvut.kbss.jopa.modelgen.classmodel;

import java.util.Arrays;
import java.util.List;

public enum AnnotationEnum {
    ID("cz.cvut.kbss.jopa.model.annotations.Id"),
    PROPERTIES("cz.cvut.kbss.jopa.model.annotations.Properties"),
    OBJECTPROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty"),
    DATAPROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLDataProperty"),
    ANNOTATIONPROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty"),
    TYPES("cz.cvut.kbss.jopa.model.annotations.Types");

    private final String annotation;

    AnnotationEnum(String annotation) {
        this.annotation = annotation;
    }

    public String getAnnotation() {
        return annotation;
    }

    public static List<AnnotationEnum> getAll() {
        return Arrays.asList(PROPERTIES, OBJECTPROPERTY, DATAPROPERTY, ANNOTATIONPROPERTY, TYPES, ID);
    }

}
