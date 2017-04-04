package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

class EntityLifecycleCallbackResolver {

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
     * @param et  AbstractIdentifiableType to process
     * @param <T> Java class represented by the entity type
     * @return Lifecycle listener manager instance
     */
    <T> EntityLifecycleListenerManager resolve(AbstractIdentifiableType<T> et) {
        final EntityLifecycleListenerManager manager = new EntityLifecycleListenerManager();
        resolveLifecycleCallbacks(et, manager);
        if (et.getSupertype() != null) {
            manager.setParent(et.getSupertype().getLifecycleListenerManager());
        }
        return manager;
    }

    private <T> void resolveLifecycleCallbacks(AbstractIdentifiableType<T> type,
                                               EntityLifecycleListenerManager manager) {
        final Class<T> cls = type.getJavaType();
        for (Method m : cls.getDeclaredMethods()) {
            for (LifecycleEvent hookType : LifecycleEvent.values()) {
                if (m.getDeclaredAnnotation(hookType.getAnnotation()) != null) {
                    verifyCallbackNotAlreadyDefined(manager, type, hookType);
                    verifyListenerSignature(type, m);
                    manager.addLifecycleCallback(hookType, m);
                }
            }
        }
    }

    private <T> void verifyCallbackNotAlreadyDefined(EntityLifecycleListenerManager manager,
                                                     AbstractIdentifiableType<T> type, LifecycleEvent hookType) {
        if (manager.hasLifecycleCallback(hookType)) {
            throw MetamodelInitializationException.multipleListenersForSameLifecycleEvent(type.getJavaType(), hookType);
        }
    }

    private <T> void verifyListenerSignature(AbstractIdentifiableType<T> type, Method listener) {
        if (listener.getParameterCount() > 0) {
            throw MetamodelInitializationException.invalidArgumentsForLifecycleListener(type.getJavaType(), listener);
        }
        if (!listener.getReturnType().equals(Void.TYPE)) {
            throw MetamodelInitializationException.invalidReturnTypeForLifecycleListener(type.getJavaType(), listener);
        }
        if (Modifier.isFinal(listener.getModifiers()) || Modifier.isStatic(listener.getModifiers())) {
            throw MetamodelInitializationException.invalidLifecycleListenerModifier(type.getJavaType(), listener);
        }
    }
}
