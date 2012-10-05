package cz.cvut.kbss.jopa.owlapi;

import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = "http://new.owl#OWLClassE")
public class OWLClassE {

	@Id(generated = true)
	private URI uri;

	@OWLDataProperty(iri = "http://E-stringAttribute")
	private String stringAttribute;

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public String getStringAttribute() {
		return stringAttribute;
	}

	public void setStringAttribute(String stringAttribute) {
		this.stringAttribute = stringAttribute;
	}

//	@Override
//	public String toString() {
//		String out = "OWLClassE: uri = " + uri;
//		out += ", stringAttribute = " + stringAttribute;
//		return out;
//	}
}
