package cz.cvut.kbss.jopa.id;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.Connection;

/**
 * Generates identifiers for entities.
 */
public interface IdentifierGenerator {

    /**
     * Generate identifier for the specified entity.
     *
     * @param entity      Entity for which to generate the identifier
     * @param entityClass Entity class
     * @param connection  Storage connection
     * @return Generated identifier
     */
    <T> Object generate(Object entity, EntityType<T> entityClass, Connection connection);
}
