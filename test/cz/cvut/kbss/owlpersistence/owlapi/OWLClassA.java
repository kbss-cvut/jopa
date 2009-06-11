package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;

import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLDataProperty;

@OWLClass(uri = "http://OWLClassA")
public class OWLClassA {

	@Id
	private URI uri;

	@OWLDataProperty(uri = "http://A-stringAttribute")
	private String stringAttribute;

	@OWLDataProperty(uri = "http://A-integerAttribute")
	private Integer integerAttribute;

	@OWLDataProperty(uri = "http://A-doubleAttribute")
	private Double doubleAttribute;

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

	public void setIntegerAttribute(Integer integerAttribute) {
		this.integerAttribute = integerAttribute;
	}

	public Integer getIntegerAttribute() {
		return integerAttribute;
	}

	public void setDoubleAttribute(Double doubleAttribute) {
		this.doubleAttribute = doubleAttribute;
	}

	public Double getDoubleAttribute() {
		return doubleAttribute;
	}
}
