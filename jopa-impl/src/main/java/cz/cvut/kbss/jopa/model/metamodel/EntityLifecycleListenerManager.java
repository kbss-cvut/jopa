package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.EntityListeners;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

/**
 * Manages entity lifecycle callbacks declared either in the entity (entity lifecycle callbacks) or in its entity
 * listener (entity listener callbacks) and provides means for their invocation.
 */
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
     * Calls pre-persist callbacks for the specified instance.
     * <p>
     * These include:
     * <ul>
     * <li>Lifecycle callbacks declared by the entity or its managed ancestors,</li>
     * <li>Callbacks declared in classes referenced by {@link EntityListeners} on the entity or its ancestors.</li>
     * </ul>
     *
     * @param instance The instance for persist
     */
    public void invokePrePersistCallbacks(Object instance) {
        invokeCallbacks(instance, LifecycleEvent.PRE_PERSIST);
    }

    private void invokeCallbacks(Object instance, LifecycleEvent lifecycleEvent) {
        invokeEntityListenerCallbacks(instance, lifecycleEvent);
        invokeInternalCallbacks(instance, lifecycleEvent);
    }

    private void invokeEntityListenerCallbacks(Object instance, LifecycleEvent lifecycleEvent) {
        if (parent != null) {
            parent.invokeEntityListenerCallbacks(instance, lifecycleEvent);
        }
        if (entityListeners != null) {
            entityListeners.forEach(listener -> {
                getEntityListenerCallback(listener, lifecycleEvent).ifPresent(method -> {
                    if (!method.isAccessible()) {
                        method.setAccessible(true);
                    }
                    try {
                        method.invoke(listener, instance);
                    } catch (IllegalAccessException | InvocationTargetException e) {
                        throw new OWLPersistenceException("Unable to invoke entity listener method " + method, e);
                    }
                });
            });
        }
    }

    private Optional<Method> getEntityListenerCallback(Object listener, LifecycleEvent lifecycleEvent) {
        final Map<LifecycleEvent, Method> callbacks = entityListenerCallbacks.get(listener);
        return Optional.ofNullable(callbacks.get(lifecycleEvent));
    }

    private void invokeInternalCallbacks(Object instance, LifecycleEvent lifecycleEvent) {
        if (parent != null) {
            parent.invokeInternalCallbacks(instance, lifecycleEvent);
        }
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
    }

    /**
     * Calls post-persist callbacks for the specified instance.
     * <p>
     * These include:
     * <ul>
     * <li>Lifecycle callbacks declared by the entity or its managed ancestors,</li>
     * <li>Callbacks declared in classes referenced by {@link EntityListeners} on the entity or its ancestors.</li>
     * </ul>
     *
     * @param instance The newly persisted instance
     */
    public void invokePostPersistCallbacks(Object instance) {
        invokeCallbacks(instance, LifecycleEvent.POST_PERSIST);
    }

    /**
     * Calls post-load callbacks for the specified instance.
     * <p>
     * These include:
     * <ul>
     * <li>Lifecycle callbacks declared by the entity or its managed ancestors,</li>
     * <li>Callbacks declared in classes referenced by {@link EntityListeners} on the entity or its ancestors.</li>
     * </ul>
     *
     * @param instance The loaded instance
     */
    public void invokePostLoadCallbacks(Object instance) {
        invokeCallbacks(instance, LifecycleEvent.POST_LOAD);
    }

    /**
     * Calls pre-update callbacks for the specified instance.
     * <p>
     * These include:
     * <ul>
     * <li>Lifecycle callbacks declared by the entity or its managed ancestors,</li>
     * <li>Callbacks declared in classes referenced by {@link EntityListeners} on the entity or its ancestors.</li>
     * </ul>
     *
     * @param instance The updated instance
     */
    public void invokePreUpdateCallbacks(Object instance) {
        invokeCallbacks(instance, LifecycleEvent.PRE_UPDATE);
    }

    /**
     * Calls post-update callbacks for the specified instance.
     * <p>
     * These include:
     * <ul>
     * <li>Lifecycle callbacks declared by the entity or its managed ancestors,</li>
     * <li>Callbacks declared in classes referenced by {@link EntityListeners} on the entity or its ancestors.</li>
     * </ul>
     *
     * @param instance The updated instance
     */
    public void invokePostUpdateCallbacks(Object instance) {
        invokeCallbacks(instance, LifecycleEvent.POST_UPDATE);
    }

    /**
     * Calls pre-remove callbacks for the specified instance.
     * <p>
     * These include:
     * <ul>
     * <li>Lifecycle callbacks declared by the entity or its managed ancestors,</li>
     * <li>Callbacks declared in classes referenced by {@link EntityListeners} on the entity or its ancestors.</li>
     * </ul>
     *
     * @param instance The instance for removal
     */
    public void invokePreRemoveCallbacks(Object instance) {
        invokeCallbacks(instance, LifecycleEvent.PRE_REMOVE);
    }

    /**
     * Calls post-remove callbacks for the specified instance.
     * <p>
     * These include:
     * <ul>
     * <li>Lifecycle callbacks declared by the entity or its managed ancestors,</li>
     * <li>Callbacks declared in classes referenced by {@link EntityListeners} on the entity or its ancestors.</li>
     * </ul>
     *
     * @param instance The removed instance
     */
    public void invokePostRemoveCallbacks(Object instance) {
        invokeCallbacks(instance, LifecycleEvent.POST_REMOVE);
    }

    void setParent(EntityLifecycleListenerManager parent) {
        this.parent = parent;
    }

    void addEntityListener(Object entityListener) {
        assert entityListener != null;
        
        if (entityListeners == null) {
            this.entityListeners = new ArrayList<>();
        }
        entityListeners.add(entityListener);
        if (entityListenerCallbacks == null) {
            this.entityListenerCallbacks = new HashMap<>();
        }
        entityListenerCallbacks.put(entityListener, new EnumMap<>(LifecycleEvent.class));
    }

    void addLifecycleCallback(LifecycleEvent event, Method callback) {
        assert event != null;
        assert callback != null;

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
        assert listener != null;
        assert event != null;
        assert callback != null;
        assert entityListenerCallbacks.containsKey(listener);

        entityListenerCallbacks.get(listener).put(event, callback);
    }

    boolean hasEntityListenerCallback(Object listener, LifecycleEvent event) {
        return entityListenerCallbacks != null && entityListenerCallbacks.containsKey(listener) &&
                entityListenerCallbacks.get(listener).containsKey(event);
    }
}
