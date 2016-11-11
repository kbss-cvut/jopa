package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.Connection;

/**
 * Root of the entity loading strategies.
 */
abstract class EntityInstanceLoader {

    final Connection storageConnection;
    final Metamodel metamodel;

    final AxiomDescriptorFactory descriptorFactory;
    final EntityConstructor entityBuilder;

    EntityInstanceLoader(Connection storageConnection, Metamodel metamodel, AxiomDescriptorFactory descriptorFactory,
                         EntityConstructor entityBuilder) {
        this.storageConnection = storageConnection;
        this.metamodel = metamodel;
        this.descriptorFactory = descriptorFactory;
        this.entityBuilder = entityBuilder;
    }

    /**
     * Loads entity based on the specified loading parameters.
     *
     * @param loadingParameters Instance loading parameters
     * @return The loaded instance (possibly {@code null})
     */
    abstract <T> T loadEntity(LoadingParameters<T> loadingParameters);
}
