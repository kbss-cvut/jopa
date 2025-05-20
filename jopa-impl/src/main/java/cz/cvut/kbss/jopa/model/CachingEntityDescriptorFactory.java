package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Factory that caches created descriptors.
 * <p>
 * This assumes the descriptors are not modified by the caller.
 */
public class CachingEntityDescriptorFactory implements EntityDescriptorFactory {

    private final EntityDescriptorFactory factory;

    private final Map<Class<?>, Descriptor> cache = new ConcurrentHashMap<>();

    public CachingEntityDescriptorFactory(EntityDescriptorFactory factory) {
        this.factory = factory;
    }

    @Override
    public <T> Descriptor createDescriptor(Class<T> cls) {
        Objects.requireNonNull(cls);
        return cache.computeIfAbsent(cls, factory::createDescriptor);
    }
}
