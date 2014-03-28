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
		if (repository.getId() < 0) {
			throw new IllegalArgumentException("Repository id cannot be less than 0.");
		}
		this.repository = repository.getId();
		this.contexts = new HashSet<>(repository.getContexts().size());
	}

	/**
	 * Copy constructor. </p>
	 * 
	 * A shallow copy of the contexts is made by this constructor, so that if
	 * contexts are removed or added to this instance it does not affect the
	 * original.
	 * 
	 * @param other
	 *            The instance to copy
	 */
	public RepositoryID(RepositoryID other) {
		if (other == null) {
			throw new NullPointerException();
		}
		this.repository = other.repository;
		this.contexts = new HashSet<>(other.contexts);
	}

	private RepositoryID(RepositoryIDBuilder builder) {
		this(builder.repo);
		contexts.addAll(builder.contexts);
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

	@Override
	public String toString() {
		final StringBuilder out = new StringBuilder("RepositoryID: repository = ")
				.append(repository).append(", contexts = ").append(contexts);
		return out.toString();
	}

	public static RepositoryIDBuilder repository(Repository repository) {
		return new RepositoryIDBuilder().repository(repository);
	}

	public static RepositoryIDBuilder context(URI contextUri) {
		return new RepositoryIDBuilder().context(contextUri);
	}

	public static RepositoryIDBuilder contexts(Set<URI> contexts) {
		return new RepositoryIDBuilder().contexts(contexts);
	}

	/**
	 * Builder for the RepositoryID class.
	 * 
	 * @author ledvima1
	 * 
	 */
	public static final class RepositoryIDBuilder {
		private Repository repo;
		private Set<URI> contexts = new HashSet<>();

		public RepositoryIDBuilder repository(Repository repository) {
			this.repo = repository;
			return this;
		}

		public RepositoryIDBuilder context(URI contextUri) {
			contexts.add(contextUri);
			return this;
		}

		public RepositoryIDBuilder contexts(Set<URI> contextUris) {
			contexts.addAll(contextUris);
			return this;
		}

		public RepositoryID build() {
			return new RepositoryID(this);
		}
	}
}
