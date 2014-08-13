package cz.cvut.kbss.ontodriver_new.model;

import java.io.Serializable;
import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

/**
 * Represents named resources, i. e. resources identified by an URI.
 * 
 * @author ledvima1
 * 
 */
public class NamedResource implements Serializable {

	private static final long serialVersionUID = 5932515448919851871L;

	private final URI identifier;

	NamedResource(URI uri) {
		this.identifier = Objects.requireNonNull(uri, ErrorUtils.constructNPXMessage("uri"));
	}

	/**
	 * Gets identifier of this resource. </p>
	 * 
	 * @return URI
	 */
	public URI getIdentifier() {
		return identifier;
	}

	/**
	 * Creates new named resource from the specified URI.
	 * 
	 * @param uri
	 *            Resource identifier
	 * @return NamedResource instance
	 */
	public static NamedResource create(URI uri) {
		return new NamedResource(uri);
	}

	/**
	 * Creates new named resource from the specified IRI.
	 * 
	 * @param iri
	 *            Resource identifier
	 * @return NamedResource instance
	 */
	public static NamedResource create(IRI iri) {
		Objects.requireNonNull(iri, ErrorUtils.constructNPXMessage("iri"));
		return new NamedResource(iri.toURI());
	}

	/**
	 * Creates new named resource from the specified string identifier.
	 * 
	 * @param iri
	 *            Resource identifier
	 * @return NamedResource instance
	 */
	public static NamedResource create(String iri) {
		return new NamedResource(URI.create(iri));
	}
}
