package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;

import java.util.Collection;

public interface DataAccessor {

    /**
     * Persists the specified test data directly into the storage.
     * <p>
     * Data are persisted using a vendor-specific storage access object unwrapped from the specified entity manager.
     *
     * @param data Data to persist
     * @param em   Means of getting vendor-specific storage access
     * @throws Exception If storage access error occurs
     */
    void persistTestData(Collection<Triple> data, EntityManager em) throws Exception;

    /**
     * Verifies that the specified data are present in the storage.
     * <p>
     * Data presence is verified using a vendor-specific storage access object unwrapped from the specified entity manager.
     *
     * @param data Data to verify
     * @param em   Means of getting vendor-specific storage access
     * @throws Exception If storage access error occurs
     */
    void verifyDataPresence(Collection<Triple> data, EntityManager em) throws Exception;
}
