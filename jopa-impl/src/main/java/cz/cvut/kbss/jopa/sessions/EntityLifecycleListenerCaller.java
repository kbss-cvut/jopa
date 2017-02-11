package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 * Takes care of calling entity lifecycle listeners.
 */
class EntityLifecycleListenerCaller {

    /**
     * Calls pre-persist listeners for the specified instance.
     *
     * @param et       Entity type of the instance
     * @param instance The instance for persist
     */
    void invokePrePersistListeners(EntityTypeImpl<?> et, Object instance) {
        invokeListeners(instance, et.getLifecycleListeners(LifecycleEvent.PRE_PERSIST));
    }

    private void invokeListeners(Object instance, List<Method> listeners) {
        for (Method listener : listeners) {
            if (!listener.isAccessible()) {
                listener.setAccessible(true);
            }
            try {
                listener.invoke(instance);
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new OWLPersistenceException("Unable to invoke method lifecycle listener " + listener, e);
            }
        }
    }

    /**
     * Calls pre-remove listeners for the specified instance.
     *
     * @param et       Entity type of the instance
     * @param instance The instance for removal
     */
    void invokePreRemoveListeners(EntityTypeImpl<?> et, Object instance) {
        invokeListeners(instance, et.getLifecycleListeners(LifecycleEvent.PRE_REMOVE));
    }

    /**
     * Calls post-persist listeners for the specified instance.
     *
     * @param et       Entity type of the instance
     * @param instance The newly persisted instance
     */
    void invokePostPersistListeners(EntityTypeImpl<?> et, Object instance) {
        invokeListeners(instance, et.getLifecycleListeners(LifecycleEvent.POST_PERSIST));
    }

    /**
     * Calls post-remove listeners for the specified instance.
     *
     * @param et       Entity type of the instance
     * @param instance The removed instance
     */
    void invokePostRemoveListeners(EntityTypeImpl<?> et, Object instance) {
        invokeListeners(instance, et.getLifecycleListeners(LifecycleEvent.POST_REMOVE));
    }

    /**
     * Calls post-load listeners for the specified instance.
     *
     * @param et       Entity type of the instance
     * @param instance The loaded instance
     */
    void invokePostLoadListeners(EntityTypeImpl<?> et, Object instance) {
        invokeListeners(instance, et.getLifecycleListeners(LifecycleEvent.POST_LOAD));
    }
}
