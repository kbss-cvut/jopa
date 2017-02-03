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
     * @param instance The instance being persisted
     */
    void callPrePersistListeners(EntityTypeImpl<?> et, Object instance) {
        final List<Method> listeners = et.getLifecycleListeners(LifecycleEvent.PRE_PERSIST);
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
}
