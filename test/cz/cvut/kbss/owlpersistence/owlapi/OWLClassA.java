package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.annotations.Id;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLClass;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLDataProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.Types;

@OWLClass(iri = "http://new.owl#OWLClassA")
public class OWLClassA {

	@Types
	private Set<String> types;
	
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

	public void setTypes(Set<String> types) {
		this.types = types;
	}

	public Set<String> getTypes() {
		return types;
	}
}
