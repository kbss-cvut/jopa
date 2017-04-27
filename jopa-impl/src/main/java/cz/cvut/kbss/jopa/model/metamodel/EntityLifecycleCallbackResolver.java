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
                    verifyLifecycleCallbackSignature(m);
                    manager.addLifecycleCallback(hookType, m);
                }
            }
        }
    }

    private void verifyCallbackNotAlreadyDefined(LifecycleEvent hookType) {
        if (manager.hasLifecycleCallback(hookType)) {
            throw new MetamodelInitializationException("The type [" + managedType.getJavaType().getName() +
                    "] has multiple lifecycle callbacks for the lifecycle event [" + hookType + "].");
        }
    }

    private void verifyLifecycleCallbackSignature(Method callback) {
        if (callback.getParameterCount() > 0) {
            throw MetamodelInitializationException
                    .invalidArgumentsForLifecycleListener(managedType.getJavaType(), callback);
        }
        if (!callback.getReturnType().equals(Void.TYPE)) {
            throw MetamodelInitializationException
                    .invalidReturnTypeForLifecycleListener(managedType.getJavaType(), callback);
        }
        if (Modifier.isFinal(callback.getModifiers()) || Modifier.isStatic(callback.getModifiers())) {
            throw MetamodelInitializationException
                    .invalidLifecycleListenerModifier(managedType.getJavaType(), callback);
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
                resolveEntityListenerCallbacks(listener, listenerType);
            } catch (InstantiationException | IllegalAccessException e) {
                throw new MetamodelInitializationException("Unable to instantiate entity listener of type "
                        + listenerType + ". The listener has to have a public no-arg constructor.");
            }
        }
    }

    private void resolveEntityListenerCallbacks(Object listener, Class<?> listenerType) {
        for (Method m : listenerType.getDeclaredMethods()) {
            for (LifecycleEvent hookType : LifecycleEvent.values()) {
                if (m.getDeclaredAnnotation(hookType.getAnnotation()) != null) {
                    verifyEntityListenerCallbackNotAlreadyDefined(listener, listenerType, hookType);
                    verifyEntityListenerCallbackSignature(listenerType, m);
                    manager.addEntityListenerCallback(listener, hookType, m);
                }
            }
        }
    }

    private void verifyEntityListenerCallbackNotAlreadyDefined(Object listener, Class<?> listenerType,
                                                               LifecycleEvent event) {
        if (manager.hasEntityListenerCallback(listener, event)) {
            throw new MetamodelInitializationException("The entity listener [" + listenerType.getName() +
                    "] has multiple callbacks for the lifecycle event [" + event + "].");
        }
    }

    private void verifyEntityListenerCallbackSignature(Class<?> listenerType, Method callback) {
        if (callback.getParameterCount() != 1) {
            throw MetamodelInitializationException
                    .invalidArgumentsForEntityListenerCallback(listenerType, callback);
        }
        final Class<?> paramType = callback.getParameterTypes()[0];
        if (!paramType.isAssignableFrom(Object.class) && !paramType.isAssignableFrom(managedType.getJavaType())) {
            throw MetamodelInitializationException
                    .invalidEntityListenerCallbackParameterType(managedType.getJavaType(), listenerType, callback);
        }
        if (!callback.getReturnType().equals(Void.TYPE)) {
            throw MetamodelInitializationException.invalidReturnTypeForEntityListenerCallback(listenerType, callback);
        }
        if (Modifier.isFinal(callback.getModifiers()) || Modifier.isStatic(callback.getModifiers())) {
            throw MetamodelInitializationException.invalidEntityListenerCallbackModifier(listenerType, callback);
        }
    }
}
