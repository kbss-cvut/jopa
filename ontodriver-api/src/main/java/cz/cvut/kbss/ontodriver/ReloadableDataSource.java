package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Allows to reload data from the underlying storage.
 * <p>
 * This is especially meant for reloading data from text files (e.g. OWL, TTL), which might have been changed externally
 * and the application may want to reload the file contents so that it works with the most recent data.
 */
public interface ReloadableDataSource extends DataSource {

    /**
     * Reloads data from the underlying storage, if possible.
     * <p>
     * Does nothing if the underlying storage does not support reloading or it is not necessary (e.g. when connected
     * to a remote triple store).
     *
     * @throws OntoDriverException   If an error occurs when reloading data
     * @throws IllegalStateException If called on a closed data source
     */
    void reload() throws OntoDriverException;
}
