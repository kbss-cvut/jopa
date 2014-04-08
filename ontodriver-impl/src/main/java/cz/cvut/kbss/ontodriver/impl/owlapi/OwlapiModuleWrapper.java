package cz.cvut.kbss.ontodriver.impl.owlapi;

import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

interface OwlapiModuleWrapper {

	/**
	 * Tries to get entity from the second level cache.
	 * 
	 * @param cls
	 *            Entity class
	 * @param primaryKey
	 *            Primary key
	 * @return Matching entity or {@code null}
	 */
	public <T> T getEntityFromCache(Class<T> cls, Object primaryKey);

	/**
	 * Returns cloned ontology structures that can be manipulated without
	 * affecting the original data.
	 * 
	 * @return OwlapiConnectorDataHolder
	 * @throws OntoDriverException
	 *             If an error during cloning occurs
	 */
	public OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException;

	/**
	 * Returns the original ontology structures directly from the connector.
	 * 
	 * @return OwlapiConnectorDataHolder
	 */
	public OwlapiConnectorDataHolder getOntologyData();

	/**
	 * Retrieves a new primary key number and increments the internal counter.
	 * 
	 * @return primary key number
	 */
	public int getNewPrimaryKey();

	/**
	 * Increments the primary key counter for this module's context.
	 */
	public void incrementPrimaryKeyCounter();

	/**
	 * Returns metamodel associated with this module wrapper.
	 * 
	 * @return {@code Metamodel}
	 */
	public Metamodel getMetamodel();

	/**
	 * Gets identifier of repository managed by this storage module.
	 * 
	 * @return Repository identifier
	 */
	public RepositoryID getRepositoryIdentifier();
}
