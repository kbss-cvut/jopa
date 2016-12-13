package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;
import cz.cvut.kbss.jopa.oom.metamodel.PolymorphicEntityTypeResolver;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Set;

class TwoStepInstanceLoader extends EntityInstanceLoader {

    TwoStepInstanceLoader(TwoStepInstanceLoaderBuilder builder) {
        super(builder);
    }

    @Override
    <T> T loadEntity(LoadingParameters<T> loadingParameters) {
        final NamedResource individual = NamedResource.create(loadingParameters.getIdentifier());
        final EntityTypeImpl<T> rootEt = metamodel.entity(loadingParameters.getEntityType());
        try {
            final Set<Axiom<URI>> types = storageConnection.types().getTypes(individual,
                    loadingParameters.getDescriptor().getContext(), false);
            final EntityType<? extends T> et =
                    new PolymorphicEntityTypeResolver<>(individual, rootEt, types).determineActualEntityType();
            if (et == null) {
                return null;
            }
            return loadInstance(loadingParameters, et);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    static TwoStepInstanceLoaderBuilder builder() {
        return new TwoStepInstanceLoaderBuilder();
    }

    static class TwoStepInstanceLoaderBuilder extends EntityInstanceLoaderBuilder {

        @Override
        EntityInstanceLoader build() {
            return new TwoStepInstanceLoader(this);
        }
    }
}
