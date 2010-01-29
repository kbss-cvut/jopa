package cz.cvut.kbss.owlpersistence.model;

import java.net.URI;
import java.net.URISyntaxException;

public class IRI implements AnnotationValue {

	final String iri;

	public static IRI create(final String s) {
		return new IRI(s);
	}

	IRI(String iri) {
		this.iri = iri;
	}

	public URI toURI() throws URISyntaxException {
		return URI.create(iri);
	}

	public String toString() {
		return iri;
	}
}
