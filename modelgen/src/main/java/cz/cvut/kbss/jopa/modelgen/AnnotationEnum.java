package cz.cvut.kbss.jopa.modelgen;

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

}
