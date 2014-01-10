package cz.cvut.kbss.jopa.test.utils;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

/**
 * Persistent storage configuration.
 * 
 * @author ledvima1
 * 
 */
public abstract class StorageConfig {

	protected String name;
	protected String directory;

	public StorageConfig() {
	}

	public void setName(String name) {
		assert name != null;
		this.name = name;
	}

	public void setDirectory(String directory) {
		assert directory != null;
		this.directory = directory;
	}

	/**
	 * Creates ontology storage properties for this storage configuration. </p>
	 * 
	 * This method performs any necessary tasks before creating the storage
	 * properties (typically deleting the old data).
	 * 
	 * @param index
	 *            Index appended to file/folder name, used when multiple
	 *            storages of the same type can occur
	 * 
	 * @return OntologyStorageProperties
	 */
	public abstract OntologyStorageProperties createStorageProperties(int index);
}
