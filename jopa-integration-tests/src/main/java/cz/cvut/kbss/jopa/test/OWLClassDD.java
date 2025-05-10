package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;

@SparqlResultSetMapping(name = OWLClassD.MAPPING_NAME, entities = {
        @EntityResult(entityClass = OWLClassD.class, fields = {
                @FieldResult(name = "uri", variable = "x"),
                @FieldResult(name = "owlClassA", variable = "y")
        })
})
@OWLClass(iri = Vocabulary.C_OWL_CLASS_D)
public class OWLClassDD implements HasUri {

    public static final String MAPPING_NAME = "OWLClassD.entityMapping";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_A, fetch = FetchType.LAZY)
    // @ParticipationConstraints({
    // @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
    // max=1)
    // })
    private OWLClassA owlClassA;

    public OWLClassDD() {
        // Default public no-arg constructor
    }

    public OWLClassDD(URI uri) {
        this.uri = uri;
    }

    /**
     * @param uri the uri to set
     */
    public void setUri(URI uri) {
        this.uri = uri;
    }

    /**
     * @return the uri
     */
    @Override
    public URI getUri() {
        return uri;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    @Override
    public String toString() {
        String out = "OWLClassD: uri = " + uri;
        out += ", owlClassA = " + owlClassA;
        return out;
    }
}
