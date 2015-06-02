package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.net.URI;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassK")
public class OWLClassK {

	@Id(generated = true)
	private URI uri;

	@OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasE")
	private OWLClassE owlClassE;

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public OWLClassE getOwlClassE() {
		return owlClassE;
	}

	public void setOwlClassE(OWLClassE owlClassE) {
		this.owlClassE = owlClassE;
	}

	@Override
	public String toString() {
		return "[OWLClassK: " + uri + ", owlClassE = " + owlClassE + "]";
	}
}
