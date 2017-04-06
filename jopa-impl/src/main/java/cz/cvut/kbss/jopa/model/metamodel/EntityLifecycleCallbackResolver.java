package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.annotations.EntityListeners;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

class EntityLifecycleCallbackResolver {

    private AbstractIdentifiableType<?> managedType;
    private EntityLifecycleListenerManager manager;

    /**
     * Builds an instance of {@link EntityLifecycleListenerManager} for the specified managed type.
     * <p>
     * The manager contains:
     * <ul>
     * <li>Lifecycle callbacks declared in the entity class,</li>
     * <li>EntityListener instance declared on the entity class,</li>
     * <li>Reference to parent {@link EntityLifecycleListenerManager} (if exists).</li>
     * </ul>
     *
     * @param et AbstractIdentifiableType to process
     * @return Lifecycle listener manager instance
     */
    EntityLifecycleListenerManager resolve(AbstractIdentifiableType<?> et) {
        this.managedType = et;
        this.manager = new EntityLifecycleListenerManager();
        resolveLifecycleCallbacks();
        resolveEntityListeners();
        if (et.getSupertype() != null) {
            manager.setParent(et.getSupertype().getLifecycleListenerManager());
        }
        return manager;
    }

    private void resolveLifecycleCallbacks() {
        final Class<?> cls = managedType.getJavaType();
        for (Method m : cls.getDeclaredMethods()) {
            for (LifecycleEvent hookType : LifecycleEvent.values()) {
                if (m.getDeclaredAnnotation(hookType.getAnnotation()) != null) {
                    verifyCallbackNotAlreadyDefined(hookType);
                    verifyListenerSignature(m);
                    manager.addLifecycleCallback(hookType, m);
                }
            }
        }
    }

    private void verifyCallbackNotAlreadyDefined(LifecycleEvent hookType) {
        if (manager.hasLifecycleCallback(hookType)) {
            throw MetamodelInitializationException
                    .multipleListenersForSameLifecycleEvent(managedType.getJavaType(), hookType);
        }
    }

    private void verifyListenerSignature(Method listener) {
        if (listener.getParameterCount() > 0) {
            throw MetamodelInitializationException
                    .invalidArgumentsForLifecycleListener(managedType.getJavaType(), listener);
        }
        if (!listener.getReturnType().equals(Void.TYPE)) {
            throw MetamodelInitializationException
                    .invalidReturnTypeForLifecycleListener(managedType.getJavaType(), listener);
        }
        if (Modifier.isFinal(listener.getModifiers()) || Modifier.isStatic(listener.getModifiers())) {
            throw MetamodelInitializationException
                    .invalidLifecycleListenerModifier(managedType.getJavaType(), listener);
        }
    }

    private void resolveEntityListeners() {
        final EntityListeners listenersAnn = managedType.getJavaType().getDeclaredAnnotation(EntityListeners.class);
        if (listenersAnn == null) {
            return;
        }
        for (Class<?> listenerType : listenersAnn.value()) {
            try {
                final Object listener = listenerType.newInstance();
                manager.addEntityListener(listener);
            } catch (InstantiationException | IllegalAccessException e) {
                throw new MetamodelInitializationException("Unable to instantiate entity listener of type "
                        + listenerType + ". The listener has to have a public no-arg constructor.");
            }
        }
    }
}
