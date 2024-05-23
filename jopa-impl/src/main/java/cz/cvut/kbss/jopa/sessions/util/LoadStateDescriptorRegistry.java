package cz.cvut.kbss.jopa.sessions.util;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.Function;

/**
 * Manages {@link cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor}s for a persistence context.
 */
public class LoadStateDescriptorRegistry {

    private final Map<Object, LoadStateDescriptor> registry = new IdentityHashMap<>();

    private final Function<Object, String> entityStringifier;

    public LoadStateDescriptorRegistry(Function<Object, String> entityStringifier) {
        this.entityStringifier = entityStringifier;
    }

    public void put(Object instance, LoadStateDescriptor<?> descriptor) {
        registry.put(instance, descriptor);
    }

    public <T> LoadStateDescriptor<T> get(T instance) {
        if (!contains(instance)) {
            throw new OWLPersistenceException("Fatal error, LoadStateDescriptorRegistry is missing descriptor for " + entityStringifier.apply(instance));
        }
        return registry.get(instance);
    }

    public boolean contains(Object instance) {
        return registry.containsKey(instance);
    }

    public void clear() {
        registry.clear();
    }
}
