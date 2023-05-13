package cz.test.ex;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Set;

@OWLClass(iri="")
public class TestingClassOWL {
    @Id(generated = true)
    private URI uri;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#E-stringAttribute")
    private TestingClassOWL testingClassOWL;

    @Inferred
    @Properties(fetchType = FetchType.LAZY)
    private Map<String, Set<String>> properties;
    @Properties
    private Map<URI, Set<Object>> propertie;
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute")
    private List<String> listAttribute;
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute")
    private Set<String> setAttribute;

//    @Types
//    private Set rawTypes;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

}