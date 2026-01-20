package cz.cvut.kbss.ontodriver;

/**
 * Information about the repository as a whole.
 */
public interface RepositoryMetadata {

    /**
     * Retrieves the name of this database product.
     *
     * @return Database product name
     */
    String getProductName();
}
