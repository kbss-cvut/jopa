package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

/**
 * Specifies repository and context to which an entity belongs. </p>
 * 
 * @author ledvima1
 * 
 */
public final class EntityOrigin {

	private final Integer repositoryId;
	private final URI entityContext;

	public EntityOrigin(Repository repository, URI entityContext) {
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		this.entityContext = Objects.requireNonNull(entityContext,
				ErrorUtils.constructNPXMessage("entityContext"));
		this.repositoryId = repository.getId();
	}

	public EntityOrigin(EntityDescriptor descriptor) {
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		this.repositoryId = descriptor.getRepository();
		this.entityContext = descriptor.getEntityContext();
	}

	public Integer getRepositoryId() {
		return repositoryId;
	}

	public URI getEntityContext() {
		return entityContext;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((entityContext == null) ? 0 : entityContext.hashCode());
		result = prime * result + repositoryId.hashCode();
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
		EntityOrigin other = (EntityOrigin) obj;
		if (entityContext == null) {
			if (other.entityContext != null)
				return false;
		} else if (!entityContext.equals(other.entityContext))
			return false;
		if (!repositoryId.equals(other.repositoryId))
			return false;
		return true;
	}

}
