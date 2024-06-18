package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.proxy.reference.EntityReferenceProxy;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.InvocationTargetException;

class EntityReferenceFactory {

    private final MetamodelImpl metamodel;

    private final UnitOfWork uow;

    EntityReferenceFactory(MetamodelImpl metamodel, UnitOfWork uow) {
        this.metamodel = metamodel;
        this.uow = uow;
    }

    /**
     * Creates entity reference proxy with initialized identifier and associated persistence context.
     *
     * @param loadingParameters Parameters for creating the reference
     * @param <T>               Type of the expected instance
     * @return Entity reference proxy
     */
    <T> T createReferenceProxy(LoadingParameters<T> loadingParameters) {
        assert loadingParameters != null;

        final Class<? extends T> referenceProxyClass = metamodel.getEntityReferenceProxy(loadingParameters.getEntityClass());
        try {
            final T reference = referenceProxyClass.getDeclaredConstructor().newInstance();
            assert reference instanceof EntityReferenceProxy<?>;
            final EntityReferenceProxy<?> referenceProxy = (EntityReferenceProxy<?>) reference;
            referenceProxy.setIdentifier(loadingParameters.getIdentifier());
            referenceProxy.setType((Class) loadingParameters.getEntityClass());
            referenceProxy.setPersistenceContext(uow);
            referenceProxy.setDescriptor(loadingParameters.getDescriptor());
            EntityPropertiesUtils.setIdentifier(loadingParameters.getIdentifier(), reference, metamodel.entity(loadingParameters.getEntityClass()));
            return reference;
        } catch (InstantiationException | IllegalAccessException | NoSuchMethodException |
                 InvocationTargetException e) {
            // We do not expect this to happen, as we generated the proxy class
            throw new OWLPersistenceException("Unable to instantiate entity reference proxy class " + referenceProxyClass, e);
        }
    }
}
