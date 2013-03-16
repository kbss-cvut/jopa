package cz.cvut.kbss.jopa.owlapi;

import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

@OWLClass(iri = "http://new.owl#OWLClassI")
public class OWLClassI {

	@Id
	private URI uri;

	@OWLObjectProperty(iri = "http://hasA", fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
	// @ParticipationConstraints({
	// @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
	// max=1)
	// })
	private OWLClassA owlClassA;

	/**
	 * @param uri
	 *            the uri to set
	 */
	public void setUri(URI uri) {
		this.uri = uri;
	}

	/**
	 * @return the uri
	 */
	public URI getUri() {
		return uri;
	}

	public void setOwlClassA(OWLClassA owlClassA) {
		this.owlClassA = owlClassA;
	}

	public OWLClassA getOwlClassA() {
		return owlClassA;
	}
}
