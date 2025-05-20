package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Context;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.net.URI;

@Context("https://example.com/context")
@OWLClass(iri = Vocabulary.CLASS_IRI_BASE + "classInContext")
public class ClassInContext {

    @Id(generated = true)
    private URI id;

    @OWLAnnotationProperty(iri = RDFS.LABEL)
    private String label;

    public ClassInContext() {
    }

    public ClassInContext(String label) {
        this.label = label;
    }

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public static String getClassIri() {
        return ClassInContext.class.getAnnotation(OWLClass.class).iri();
    }
}
