package cz.cvut.kbss.jopa.model;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Represents an ontology repository.
 * 
 * @author ledvima1
 * 
 */
public final class Repository {

	private static final AtomicInteger ID_COUNTER = new AtomicInteger(0);

	private final Integer id;
	private final URI physicalUri;
	private final Set<URI> contexts;

	public Repository(URI physicalUri) {
		if (physicalUri == null) {
			throw new NullPointerException();
		}
		this.id = ID_COUNTER.incrementAndGet();
		this.physicalUri = physicalUri;
		this.contexts = new HashSet<>();
	}

	// Internal identifier of the repository
	public Integer getId() {
		return id;
	}

	/**
	 * Gets physical URI of this repository.
	 * 
	 * @return physical URI
	 */
	public URI getPhysicalUri() {
		return physicalUri;
	}

	/**
	 * Gets a set of contexts available in this repository.
	 * 
	 * @return set of context URIs
	 */
	public Set<URI> getContexts() {
		return contexts;
	}

	/**
	 * Adds context URI to this repository.
	 * 
	 * @param contextUri
	 *            context URI to add
	 */
	public void addContext(URI contextUri) {
		if (contextUri == null) {
			throw new NullPointerException();
		}
		contexts.add(contextUri);
	}

	/**
	 * Checks whether a context with the specified URI exists in this
	 * repository.
	 * 
	 * @param contextUri
	 *            context URI
	 * @return {@code true} if such context is registered in this repository,
	 *         {@code false} otherwise
	 */
	public boolean containsContext(URI contextUri) {
		return contexts.contains(contextUri);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Repository other = (Repository) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}
}
