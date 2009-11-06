package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;

import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLDataProperty;

@OWLClass(iri = "http://OWLClassA")
public class OWLClassA {

	@Id
	private URI uri;

	@OWLDataProperty(iri = "http://A-stringAttribute")
	private String stringAttribute;

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

	public void setStringAttribute(String stringAttribute) {
		this.stringAttribute = stringAttribute;
	}

	public String getStringAttribute() {
		return stringAttribute;
	}
}
