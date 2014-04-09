package cz.cvut.kbss.jopa.model;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import cz.cvut.kbss.jopa.utils.ErrorUtils;

public final class RepositoryID {

	private final Repository repository;
	private final Set<URI> contexts;

	public RepositoryID(Repository repository) {
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		this.repository = repository;
		this.contexts = new HashSet<>();
	}

	/**
	 * Gets repository ID.
	 * 
	 * @return
	 */
	public Integer getRepository() {
		return repository.getId();
	}

	/**
	 * Gets contexts represented by this identifier.
	 * 
	 * @return Set of context URIs
	 */
	public Set<URI> getContexts() {
		return contexts;
	}

	/**
	 * Adds a context URI to this repository identifier.
	 * 
	 * @param context
	 *            Context URI
	 * @return This instance
	 * @throws NullPointerException
	 *             If {@code context} is {@code null}
	 */
	public RepositoryID addContext(URI context) {
		Objects.requireNonNull(context, ErrorUtils.constructNPXMessage("context"));

		contexts.add(context);
		return this;
	}

	/**
	 * Adds the specified context URIs to this repository identifier.
	 * 
	 * @param contexts
	 *            Context URIs
	 * @return This instance
	 * @throws NullPointerException
	 *             If {@code contexts} are {@code null}
	 */
	public RepositoryID addContexts(Collection<URI> contexts) {
		Objects.requireNonNull(contexts, ErrorUtils.constructNPXMessage("contexts"));

		this.contexts.addAll(contexts);
		return this;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + repository.hashCode();
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
		if (!repository.equals(other.repository))
			return false;
		return true;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("RepositoryID: physical URI = ")
				.append(repository.getPhysicalUri()).append(", contexts = ").append(contexts);
		return sb.toString();
	}
}
