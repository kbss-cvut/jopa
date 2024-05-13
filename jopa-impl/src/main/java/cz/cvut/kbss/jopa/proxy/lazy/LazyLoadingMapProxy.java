package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.CollectionFactory;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * {@link Map} proxy that triggers lazy loading when its contents is accessed.
 *
 * @param <T> Type of the object whose attribute value is this map proxy
 * @param <K> Map key type
 * @param <V> Map value type
 */
public class LazyLoadingMapProxy<T, K, V> implements LazyLoadingProxy<Map<K, V>>, Map<K, V> {

    protected final transient T owner;
    protected final transient FieldSpecification<? super T, Map<K, V>> fieldSpec;
    protected final transient UnitOfWork persistenceContext;

    public LazyLoadingMapProxy(T owner, FieldSpecification<? super T, Map<K, V>> fieldSpec,
                               UnitOfWork persistenceContext) {
        this.owner = owner;
        this.fieldSpec = fieldSpec;
        this.persistenceContext = persistenceContext;
    }

    @Override
    public Map<K, V> triggerLazyLoading() {
        if (persistenceContext == null || !persistenceContext.isActive()) {
            throw new LazyLoadingException("No active persistence context is available in lazy loading proxy for attribute "
                    + fieldSpec + " of entity " + owner);
        }
        return (Map<K, V>) persistenceContext.loadEntityField(owner, fieldSpec);
    }

    @Override
    public Map<K, V> unwrap() {
        return (Map<K, V>) CollectionFactory.createDefaultMap();
    }

    @Override
    public int size() {
        return triggerLazyLoading().size();
    }

    @Override
    public boolean isEmpty() {
        return triggerLazyLoading().isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return triggerLazyLoading().containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return triggerLazyLoading().containsValue(value);
    }

    @Override
    public V get(Object key) {
        return triggerLazyLoading().get(key);
    }

    @Override
    public V put(K key, V value) {
        return triggerLazyLoading().put(key, value);
    }

    @Override
    public V remove(Object key) {
        return triggerLazyLoading().remove(key);
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        triggerLazyLoading().putAll(m);
    }

    @Override
    public void clear() {
        triggerLazyLoading().clear();
    }

    @Override
    public Set<K> keySet() {
        return triggerLazyLoading().keySet();
    }

    @Override
    public Collection<V> values() {
        return triggerLazyLoading().values();
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        return triggerLazyLoading().entrySet();
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "[" + owner.getClass().getSimpleName() + "." + fieldSpec.getName() + "]";
    }
}
