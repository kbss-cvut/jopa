package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.oom.exceptions.EntityReconstructionException;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Collection;

/**
 * Root of the entity loading strategies.
 */
abstract class EntityInstanceLoader {

    final Connection storageConnection;
    final Metamodel metamodel;

    private final AxiomDescriptorFactory descriptorFactory;
    private final EntityConstructor entityBuilder;

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

    <T> T loadInstance(LoadingParameters<T> loadingParameters, EntityType<? extends T> et) {
        final AxiomDescriptor axiomDescriptor = descriptorFactory.createForEntityLoading(loadingParameters, et);
        try {
            final Collection<Axiom<?>> axioms = storageConnection.find(axiomDescriptor);
            if (axioms.isEmpty()) {
                return null;
            }
            return entityBuilder
                    .reconstructEntity(loadingParameters.getIdentifier(), et, loadingParameters.getDescriptor(),
                            axioms);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        } catch (InstantiationException | IllegalAccessException e) {
            throw new EntityReconstructionException(e);
        }
    }
}
