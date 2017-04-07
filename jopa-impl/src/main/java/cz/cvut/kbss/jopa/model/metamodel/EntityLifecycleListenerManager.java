package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

public class EntityLifecycleListenerManager {

    private static final EntityLifecycleListenerManager EMPTY = new EntityLifecycleListenerManager();

    private EntityLifecycleListenerManager parent;

    private final Map<LifecycleEvent, Method> lifecycleCallbacks = new EnumMap<>(LifecycleEvent.class);

    private List<Object> entityListeners;

    private Map<Object, Map<LifecycleEvent, Method>> entityListenerCallbacks;

    /**
     * Gets default instance of this manager, which contains no listeners and does nothing on invocation.
     *
     * @return Default {@link EntityLifecycleListenerManager} instance
     */
    public static EntityLifecycleListenerManager empty() {
        return EMPTY;
    }

    /**
     * Calls pre-persist listeners for the specified instance.
     *
     * @param instance The instance for persist
     */
    public void invokePrePersistListeners(Object instance) {
        invokeListeners(instance, LifecycleEvent.PRE_PERSIST);
    }

    private void invokeListeners(Object instance, LifecycleEvent lifecycleEvent) {
        if (lifecycleCallbacks.containsKey(lifecycleEvent)) {
            final Method listener = lifecycleCallbacks.get(lifecycleEvent);
            if (!listener.isAccessible()) {
                listener.setAccessible(true);
            }
            try {
                listener.invoke(instance);
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new OWLPersistenceException("Unable to invoke method lifecycle listener " + listener, e);
            }
        }
        if (parent != null) {
            parent.invokeListeners(instance, lifecycleEvent);
        }
    }

    /**
     * Calls pre-remove listeners for the specified instance.
     *
     * @param instance The instance for removal
     */
    public void invokePreRemoveListeners(Object instance) {
        invokeListeners(instance, LifecycleEvent.PRE_REMOVE);
    }

    /**
     * Calls post-persist listeners for the specified instance.
     *
     * @param instance The newly persisted instance
     */
    public void invokePostPersistListeners(Object instance) {
        invokeListeners(instance, LifecycleEvent.POST_PERSIST);
    }

    /**
     * Calls post-remove listeners for the specified instance.
     *
     * @param instance The removed instance
     */
    public void invokePostRemoveListeners(Object instance) {
        invokeListeners(instance, LifecycleEvent.POST_REMOVE);
    }

    /**
     * Calls post-load listeners for the specified instance.
     *
     * @param instance The loaded instance
     */
    public void invokePostLoadListeners(Object instance) {
        invokeListeners(instance, LifecycleEvent.POST_LOAD);
    }

    /**
     * Calls pre-update listeners for the specified instance.
     *
     * @param instance The updated instance
     */
    public void invokePreUpdateListeners(Object instance) {
        invokeListeners(instance, LifecycleEvent.PRE_UPDATE);
    }

    /**
     * Calls post-update listeners for the specified instance.
     *
     * @param instance The updated instance
     */
    public void invokePostUpdateListeners(Object instance) {
        invokeListeners(instance, LifecycleEvent.POST_UPDATE);
    }

    void setParent(EntityLifecycleListenerManager parent) {
        this.parent = parent;
    }

    void addEntityListener(Object entityListener) {
        Objects.requireNonNull(entityListener);
        if (entityListeners == null) {
            this.entityListeners = new ArrayList<>();
        }
        entityListeners.add(entityListener);
    }

    void addLifecycleCallback(LifecycleEvent event, Method callback) {
        Objects.requireNonNull(event);
        Objects.requireNonNull(callback);
        lifecycleCallbacks.put(event, callback);
    }

    Map<LifecycleEvent, Method> getLifecycleCallbacks() {
        return Collections.unmodifiableMap(lifecycleCallbacks);
    }

    boolean hasLifecycleCallback(LifecycleEvent event) {
        return lifecycleCallbacks.containsKey(event);
    }

    List<Object> getEntityListeners() {
        return entityListeners != null ? Collections.unmodifiableList(entityListeners) : Collections.emptyList();
    }

    Map<Object, Map<LifecycleEvent, Method>> getEntityListenerCallbacks() {
        return entityListenerCallbacks != null ? Collections.unmodifiableMap(entityListenerCallbacks) :
               Collections.emptyMap();
    }

    void addEntityListenerCallback(Object listener, LifecycleEvent event, Method callback) {
        Objects.requireNonNull(listener);
        Objects.requireNonNull(event);
        Objects.requireNonNull(callback);
        if (entityListenerCallbacks == null) {
            this.entityListenerCallbacks = new HashMap<>();
        }
        entityListenerCallbacks.putIfAbsent(listener, new EnumMap<>(LifecycleEvent.class));
        entityListenerCallbacks.get(listener).put(event, callback);
    }

    boolean hasEntityListenerCallback(Object listener, LifecycleEvent event) {
        return entityListenerCallbacks != null && entityListenerCallbacks.containsKey(listener) &&
                entityListenerCallbacks.get(listener).containsKey(event);
    }
}
