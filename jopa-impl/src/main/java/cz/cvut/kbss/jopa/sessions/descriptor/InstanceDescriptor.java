package cz.cvut.kbss.jopa.sessions.descriptor;

import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Describes an instance managed by a persistence context.
 */
public class InstanceDescriptor<T> {

    private final T instance;

    private final Map<FieldSpecification<? super T, ?>, LoadState> loadState;

    InstanceDescriptor(T instance, EntityType<T> et) {
        this.instance = Objects.requireNonNull(instance);
        this.loadState = mapInstanceAttributes(et);
    }

    InstanceDescriptor(T instance, InstanceDescriptor<T> other) {
        this.instance = Objects.requireNonNull(instance);
        this.loadState = new HashMap<>(other.loadState);
    }

    private Map<FieldSpecification<? super T, ?>, LoadState> mapInstanceAttributes(EntityType<T> et) {
        final Map<FieldSpecification<? super T, ?>, LoadState> map = new HashMap<>();
        for (FieldSpecification<? super T, ?> fs : et.getFieldSpecifications()) {
            map.put(fs, LoadState.NOT_LOADED);
        }
        map.put(et.getIdentifier(), LoadState.LOADED);
        return map;
    }

    public LoadState isLoaded() {
        boolean unknownFound = false;
        for (Map.Entry<FieldSpecification<? super T, ?>, LoadState> e : loadState.entrySet()) {
            if (e.getKey().getFetchType() == FetchType.LAZY) {
                continue;
            }
            if (e.getValue() == LoadState.NOT_LOADED) {
                return LoadState.NOT_LOADED;
            } else if (e.getValue() == LoadState.UNKNOWN) {
                unknownFound = true;
            }
        }
        return unknownFound ? LoadState.UNKNOWN : LoadState.LOADED;
    }

    public LoadState isLoaded(FieldSpecification<? super T, ?> attribute) {
        return loadState.getOrDefault(Objects.requireNonNull(attribute), LoadState.UNKNOWN);
    }

    public Object getInstance() {
        return instance;
    }

    public void setLoaded(FieldSpecification<? super T, ?> fs, LoadState state) {
        Objects.requireNonNull(fs);
        Objects.requireNonNull(state);
        assert fs.getDeclaringType().getJavaType().isAssignableFrom(instance.getClass());

        loadState.put(fs, state);
    }

    @Override
    public String toString() {
        return "InstanceDescriptor{" +
                "instance=" + instance +
                ", loadState=" + loadState +
                '}';
    }
}
