package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;

/**
 * Loads entities which do not require polymorphic handling.
 */
class DefaultInstanceLoader extends EntityInstanceLoader {

    DefaultInstanceLoader(DefaultInstanceLoaderBuilder builder) {
        super(builder);
    }

    @Override
    <T> T loadEntity(LoadingParameters<T> loadingParameters) {
        final EntityType<T> et = metamodel.entity(loadingParameters.getEntityType());
        return loadInstance(loadingParameters, et);
    }

    static DefaultInstanceLoaderBuilder builder() {
        return new DefaultInstanceLoaderBuilder();
    }

    static class DefaultInstanceLoaderBuilder extends EntityInstanceLoaderBuilder {

        @Override
        EntityInstanceLoader build() {
            return new DefaultInstanceLoader(this);
        }
    }
}
