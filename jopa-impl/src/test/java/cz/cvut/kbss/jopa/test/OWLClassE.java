package cz.cvut.kbss.jopa.test;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE")
public class OWLClassE {

	private static final String STR_ATT_FIELD = "stringAttribute";

	@Id(generated = true)
	private URI uri;

	@OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#E-stringAttribute")
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

	public static String getClassIri() {
		return OWLClassE.class.getAnnotation(OWLClass.class).iri();
	}
	
	public static Field getStrAttField() throws NoSuchFieldException, SecurityException {
		return OWLClassE.class.getDeclaredField(STR_ATT_FIELD);
	}

	// @Override
	// public String toString() {
	// String out = "OWLClassE: uri = " + uri;
	// out += ", stringAttribute = " + stringAttribute;
	// return out;
	// }
}
