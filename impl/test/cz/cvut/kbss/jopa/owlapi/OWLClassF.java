package cz.cvut.kbss.jopa.owlapi;

import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = "http://new.owl#OWLClassF")
public class OWLClassF {

	@Id
	private URI uri;

	@OWLDataProperty(iri = "http://F-secondStringAttribute", inferred = true)
	private String secondStringAttribute;

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public String getSecondStringAttribute() {
		return secondStringAttribute;
	}

	public void setSecondStringAttribute(String secondStringAttribute) {
		this.secondStringAttribute = secondStringAttribute;
	}
}
