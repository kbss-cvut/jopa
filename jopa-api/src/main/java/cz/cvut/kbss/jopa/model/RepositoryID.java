package cz.cvut.kbss.jopa.model;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

/**
 * Identifier of an ontology repository. </p>
 * 
 * Instances of this class are passed to EntityManager methods as identifiers of
 * the repository and contexts on which they should operate.
 * 
 * @author ledvima1
 * 
 */
public final class RepositoryID {

	private final Integer repository;
	private final Set<URI> contexts;

	public RepositoryID(Repository repository) {
		if (repository == null) {
			throw new NullPointerException();
		}
		this.repository = repository.getId();
		this.contexts = new HashSet<>(repository.getContexts().size());
	}

	/**
	 * Gets context URIs selected in this repository identifier.
	 * 
	 * @return set of context URIs
	 */
	public Set<URI> getContexts() {
		return contexts;
	}

	/**
	 * Gets repository identifier.
	 * 
	 * @return
	 */
	public Integer getRepository() {
		return repository;
	}

	/**
	 * Adds context to this repository identifier.
	 * 
	 * @param contextUri
	 *            context URI
	 */
	public void addContext(URI contextUri) {
		if (contextUri == null) {
			throw new NullPointerException();
		}
		contexts.add(contextUri);
	}

	/**
	 * Adds the specified contexts to this repository identifier.
	 * 
	 * @param contextUris
	 *            set of context URIs
	 */
	public void addContexts(Set<URI> contextUris) {
		if (contextUris == null) {
			throw new NullPointerException();
		}
		contexts.addAll(contextUris);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((repository == null) ? 0 : repository.hashCode());
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
		RepositoryID other = (RepositoryID) obj;
		if (repository == null) {
			if (other.repository != null)
				return false;
		} else if (!repository.equals(other.repository))
			return false;
		return true;
	}
}
