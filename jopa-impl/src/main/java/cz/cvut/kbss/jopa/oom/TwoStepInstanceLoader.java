package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.Connection;

class TwoStepInstanceLoader extends EntityInstanceLoader {

    TwoStepInstanceLoader(Connection storageConnection, Metamodel metamodel, AxiomDescriptorFactory descriptorFactory,
                          EntityConstructor entityBuilder) {
        super(storageConnection, metamodel, descriptorFactory, entityBuilder);
    }

    @Override
    <T> T loadEntity(LoadingParameters<T> loadingParameters) {
        return null;
    }
}
