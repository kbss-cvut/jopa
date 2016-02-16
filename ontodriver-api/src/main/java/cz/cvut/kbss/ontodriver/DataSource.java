package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Map;

/**
 * Represents an ontology data source. </p>
 * <p/>
 * This could be either a single ontology or a storage with multiple named
 * graphs or ontology modules.
 * <p/>
 * The implementations are required to have a public no-arg constructor. Connection settings are passed through the setter methods.
 *
 * @author kidney
 */
public interface DataSource extends Closeable {

    /**
     * Requests a connection to the underlying data source. </p>
     *
     * @return A {@code Connection} to the data source
     * @throws OntoDriverException If an ontology access error occurs
     */
    public Connection getConnection() throws OntoDriverException;

    /**
     * Sets storage properties for this data source.
     * <p/>
     * Note that if the data source is already connected to the underlying storage, the outcome of this method is not specified and is likely to have no effect.
     *
     * @param storageProperties The properties to use
     * @throws OntoDriverException If an ontology access error occurs
     */
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException;

    /**
     * Sets additional configuration properties on the data source.
     *
     * @param properties Map of properties
     * @throws OntoDriverException If an ontology access error occurs
     */
    public void setProperties(Map<String, String> properties) throws OntoDriverException;
}
