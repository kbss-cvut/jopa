package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;

import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLDataProperty;

@OWLClass(iri = "http://OWLClassB")
public class OWLClassB {

	@Id
	private URI uri;

	@OWLDataProperty(iri = "http://B-stringAttribute")
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
