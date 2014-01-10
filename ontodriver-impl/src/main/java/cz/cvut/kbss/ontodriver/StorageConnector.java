package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Represents connector to the physical ontology storage, e. g. OWLDB, OWLIM.
 * 
 * @author kidney
 * 
 */
public interface StorageConnector extends Closeable {

	/**
	 * Reloads the ontology from the storage.
	 * 
	 * @throws OntoDriverException
	 *             If an error during reloading occurs
	 */
	public void reload() throws OntoDriverException;
}
