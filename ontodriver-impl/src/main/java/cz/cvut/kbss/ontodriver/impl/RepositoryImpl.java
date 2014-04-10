package cz.cvut.kbss.ontodriver.impl;

import java.net.URI;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

/**
 * Represents an ontology repository.
 * 
 * @author ledvima1
 * 
 */
public final class RepositoryImpl implements Repository {

	private static final AtomicInteger ID_COUNTER = new AtomicInteger(0);

	private final Integer id;
	private final URI physicalUri;
	private final Set<URI> contexts;

	public RepositoryImpl(URI physicalUri) {
		this.physicalUri = Objects.requireNonNull(physicalUri,
				ErrorUtils.constructNPXMessage("physicalUri"));
		this.id = ID_COUNTER.getAndIncrement();
		this.contexts = new HashSet<>();
	}

	@Override
	public Integer getId() {
		return id;
	}

	@Override
	public URI getPhysicalUri() {
		return physicalUri;
	}

	@Override
	public Set<URI> getContexts() {
		return contexts;
	}

	@Override
	public void addContext(URI contextUri) {
		if (contextUri == null) {
			throw new NullPointerException();
		}
		contexts.add(contextUri);
	}

	@Override
	public boolean containsContext(URI contextUri) {
		return contexts.contains(contextUri);
	}

	@Override
	public RepositoryID createIdentifier() {
		return new RepositoryID(this);
	}

	@Override
	public EntityDescriptor createDescriptor() {
		return new EntityDescriptor(this);
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
		RepositoryImpl other = (RepositoryImpl) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}

	@Override
	public String toString() {
		final StringBuilder out = new StringBuilder("Repository: id = ").append(id)
				.append(", physicalUri = ").append(physicalUri).append(", contexts = ")
				.append(contexts);
		return out.toString();
	}
}
