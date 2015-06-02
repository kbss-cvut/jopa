package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassG")
public class OWLClassG {

	@Id
	private URI uri;

	@OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasH", fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	// @ParticipationConstraints({
	// @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
	// max=1)
	// })
	private OWLClassH owlClassH;

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

	public void setOwlClassH(OWLClassH owlClassH) {
		this.owlClassH = owlClassH;
	}

	public OWLClassH getOwlClassH() {
		return owlClassH;
	}

	@Override
	public String toString() {
		return "OWLClassG{" +
				"uri=" + uri +
				", owlClassH=" + owlClassH +
				'}';
	}
}
