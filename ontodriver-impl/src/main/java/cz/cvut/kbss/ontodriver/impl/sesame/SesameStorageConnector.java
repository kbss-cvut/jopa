package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.List;

import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public interface SesameStorageConnector extends StorageConnector {

	/**
	 * Returns a view of the current state of the underlying ontology. </p>
	 * 
	 * It is up to the connector and its configuration whether the returned view
	 * will contain a cached copy or whether it will use direct repository
	 * connection.
	 * 
	 * @return Data holder containing ontology
	 * @throws OntoDriverException
	 *             If an error occurs
	 * @see {@link SesameOntologyDataHolder}
	 */
	public SesameOntologyDataHolder getOntologyData() throws OntoDriverException;

	/**
	 * Applies the specified changes to the underlying ontology.
	 * 
	 * @param changes
	 *            The changes to apply
	 * @throws OntoDriverException
	 *             If an error occurs
	 */
	public void applyChanges(List<SesameChange> changes) throws OntoDriverException;

	/**
	 * Get the current count of explicit subjects in the ontology. </p>
	 * 
	 * Note that this method may return 0 as long as no connection to the
	 * repository has been established.
	 * 
	 * @return subject count
	 */
	public long getSubjectCount();
}
