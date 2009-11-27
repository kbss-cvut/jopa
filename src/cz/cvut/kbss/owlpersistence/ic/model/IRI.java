package cz.cvut.kbss.owlpersistence.ic.model;

import java.net.URI;
import java.net.URISyntaxException;

public interface IRI extends AnnotationValue {

	public boolean canAsURI();

	public URI toURI() throws URISyntaxException;

}
