package cz.cvut.kbss.jopa.model;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import cz.cvut.kbss.jopa.sessions.EntityOrigin;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

/**
 * Describes storage properties of an entity. </p>
 * 
 * I. e. repository and context in which the individual is stored and optionally
 * context of each attribute (in the same repository).
 * 
 * @author ledvima1
 * 
 */
public final class EntityDescriptor {

	private final Integer repository;
	private final URI repositoryUri;
	private URI entityContext;
	private Map<String, URI> fieldContexts;

	private EntityOrigin origin;

	public EntityDescriptor(Repository repository) {
		Objects.requireNonNull(repository, "Argument 'repository' cannot be null.");
		if (repository.getId() < 0) {
			throw new IllegalArgumentException("Repository id cannot be less than 0.");
		}

		this.repository = repository.getId();
		this.repositoryUri = repository.getPhysicalUri();
		this.fieldContexts = new HashMap<>();
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
	public EntityDescriptor(EntityDescriptor other) {
		Objects.requireNonNull(other, "Argument 'repository' cannot be null.");

		this.repository = other.repository;
		this.repositoryUri = other.repositoryUri;
		this.entityContext = other.entityContext;
		this.fieldContexts = new HashMap<>(other.fieldContexts);
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
	 * Gets repository physical URI.
	 * 
	 * @return URI
	 */
	public URI getRepositoryUri() {
		return repositoryUri;
	}

	/**
	 * Gets context in which the entity (individual with this entity's URI) is
	 * stored.
	 * 
	 * @return Context URI
	 */
	public URI getEntityContext() {
		return entityContext;
	}

	/**
	 * Sets entity context. </p>
	 * 
	 * The context URI can be null, in which case the entity is stored in the
	 * default context of the repository.
	 * 
	 * @param contextUri
	 *            Context URI
	 * @return this instance
	 * @see #getEntityContext()
	 */
	public EntityDescriptor setEntityContext(URI contextUri) {
		this.entityContext = contextUri;
		return this;
	}

	/**
	 * Gets a map of field names and URIs of contexts in which the field values
	 * are stored.
	 * 
	 * @return Map
	 */
	public Map<String, URI> getFieldContexts() {
		return fieldContexts;
	}

	/**
	 * Gets URI of context in which field of the specified name is stored. </p>
	 * 
	 * If the field context is not explicitly set, the entity's context is
	 * returned.
	 * 
	 * @param fieldName
	 *            Field name
	 * @return Context URI (can be {@code null})
	 * @throws NullPointerException
	 *             If {@code fieldName} is {@code null}
	 */
	public URI getFieldContext(String fieldName) {
		Objects.requireNonNull(fieldName, ErrorUtils.constructNPXMessage("fieldName"));

		return fieldContexts.containsKey(fieldName) ? fieldContexts.get(fieldName) : entityContext;
	}

	/**
	 * Sets context into which the field value will be stored.
	 * 
	 * @param fieldName
	 *            Field name
	 * @param context
	 *            Context URI
	 * @return This instance
	 */
	public EntityDescriptor setFieldContext(String fieldName, URI context) {
		Objects.requireNonNull(fieldName, ErrorUtils.constructNPXMessage("fieldName"));
		Objects.requireNonNull(context, ErrorUtils.constructNPXMessage("context"));

		fieldContexts.put(fieldName, context);
		return this;
	}

	/**
	 * Gets origin for an entity described by this descriptor.
	 * 
	 * @return EntityOrigin
	 */
	public EntityOrigin getEntityOrigin() {
		if (origin == null) {
			this.origin = new EntityOrigin(this);
		}
		return origin;
	}

	@Override
	public String toString() {
		final StringBuilder out = new StringBuilder("EntityDescriptor: repository = ")
				.append(repositoryUri).append(", entityContexts = ").append(entityContext)
				.append(", fieldContext = ").append(fieldContexts);
		return out.toString();
	}

}
