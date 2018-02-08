package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;

import java.util.Map;

public interface PersistenceFactory {

    /**
     * Builds persistence unit and acquires an entity manager instance.
     *
     * @param repositoryName Repository name, used to create physical and logical URI of the storage
     * @param cacheEnabled   Whether to enable second-level cache
     * @param properties     Additional configuration properties
     * @return Entity manager instance
     */
    EntityManager getEntityManager(String repositoryName, boolean cacheEnabled, Map<String, String> properties);
}
