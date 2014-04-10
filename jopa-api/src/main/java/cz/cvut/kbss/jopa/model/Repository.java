package cz.cvut.kbss.jopa.model;

import java.net.URI;
import java.util.Set;

/**
 * Represents an ontology repository.
 * 
 * @author ledvima1
 * 
 */
public interface Repository {

	// Internal identifier of the repository
	public Integer getId();

	/**
	 * Gets physical URI of this repository.
	 * 
	 * @return physical URI
	 */
	public URI getPhysicalUri();

	/**
	 * Gets a set of contexts available in this repository.
	 * 
	 * @return set of context URIs
	 */
	public Set<URI> getContexts();

	/**
	 * Adds context URI to this repository.
	 * 
	 * @param contextUri
	 *            context URI to add
	 */
	public void addContext(URI contextUri);

	/**
	 * Checks whether a context with the specified URI exists in this
	 * repository.
	 * 
	 * @param contextUri
	 *            context URI
	 * @return {@code true} if such context is registered in this repository,
	 *         {@code false} otherwise
	 */
	public boolean containsContext(URI contextUri);

	/**
	 * Creates new repository identifier for this repository. </p>
	 * 
	 * No contexts are added to the identifier.
	 * 
	 * @return New repository identifier
	 */
	public RepositoryID createIdentifier();

	/**
	 * Creates new entity descriptor from this repository.
	 * 
	 * @return New entity descriptor
	 */
	public EntityDescriptor createDescriptor();

}