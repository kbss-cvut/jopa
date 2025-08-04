package cz.test.ex;

import cz.cvut.kbss.jopa.model.annotations.Context;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.net.URI;


@Context(TestingClassWithContext.CONTEXT)
@OWLClass(iri = TestingClassWithContext.CONTEXT)
public class TestingClassWithContext {

    public static final String CONTEXT = "http://example.org/TestingClassWithContext";

    @Id(generated = true)
    private URI id;

    @OWLAnnotationProperty(iri = RDFS.LABEL)
    private String label;
}
