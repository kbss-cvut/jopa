package cz.cvut.kbss.jopa.id;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;

/**
 * Generates identifiers for entities.
 */
public interface IdentifierGenerator {

    /**
     * Generate identifier for the specified entity.
     *
     * @param entity     Entity for which to generate the identifier
     * @param metamodel  Entity class metamodel
     * @param connection Storage connection
     * @return Generated identifier
     */
    Object generate(Object entity, Metamodel metamodel, Connection connection);
}
