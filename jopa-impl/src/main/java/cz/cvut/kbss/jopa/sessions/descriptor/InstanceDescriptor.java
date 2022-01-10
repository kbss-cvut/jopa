/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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

    public LoadState isLoaded(FieldSpecification<?, ?> attribute) {
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
