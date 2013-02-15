package cz.cvut.kbss.ontodriver;

import java.util.List;

/**
 * Manages the whole underlying ontology storage which can consist of several
 * ontologies or modules (OWL modules or RDF named graphs). </p>
 * 
 * The responsibility of a {@code StorageManager} is to provide unified access
 * to both OWL and RDF storages and thus represent a generic facade to multiple
 * types of ontology storages.
 * 
 * @author kidney
 * 
 */
public abstract class StorageManager {

	/**
	 * Returns a list of all available contexts this {@code StorageManager} is
	 * managing. </p>
	 * 
	 * The returned list can never be empty since there always has to be at
	 * least the default context of the loaded ontology.
	 * 
	 * @return List of contexts
	 */
	public abstract List<Context> getAvailableContexts();

	// TODO The storage manager should provide all the methods needed by
	// Connection
}
