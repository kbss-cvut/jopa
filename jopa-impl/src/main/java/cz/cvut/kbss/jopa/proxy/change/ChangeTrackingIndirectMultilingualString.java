/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.proxy.change;

import cz.cvut.kbss.jopa.proxy.IndirectWrapper;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Wraps a {@link MultilingualString} so that calls to modifying operations are intercepted and reported to the
 * persistence context (if necessary).
 */
public class ChangeTrackingIndirectMultilingualString extends MultilingualString implements IndirectWrapper {

    private final transient Object owner;
    private final transient Field field;
    private final transient UnitOfWork persistenceContext;

    /**
     * Create new indirect multilingual string backed by the specified referenced {@link MultilingualString}.
     *
     * @param owner            Owner of the string
     * @param f                The field holding this string
     * @param uow              Persistence context the owner belongs to
     * @param referencedString The string to reference
     * @throws NullPointerException If the {@code referencedString} is null
     */
    public ChangeTrackingIndirectMultilingualString(Object owner, Field f, UnitOfWork uow, MultilingualString referencedString) {
        this.owner = owner;
        this.field = f;
        this.persistenceContext = Objects.requireNonNull(uow);
        // Use super.putAll so that the initialization does not trigger change notification
        super.putAll(Objects.requireNonNull(referencedString));
    }

    private void notifyPersistenceContext() {
        assert persistenceContext != null;
        if (persistenceContext.isInTransaction() && !persistenceContext.isFlushingChanges()) {
            persistenceContext.attributeChanged(owner, field);
        }
    }

    @Override
    public MultilingualString set(String language, String value) {
        // Call super.put directly so that the change is not reported twice (super.set calls put virtually)
        super.put(language, Objects.requireNonNull(value));
        notifyPersistenceContext();
        return this;
    }

    @Override
    public MultilingualString set(String value) {
        super.put(null, Objects.requireNonNull(value));
        notifyPersistenceContext();
        return this;
    }

    @Override
    public String put(String key, String value) {
        final String previous = super.put(key, value);
        notifyPersistenceContext();
        return previous;
    }

    @Override
    public String putIfAbsent(String key, String value) {
        final String previous = super.putIfAbsent(key, value);
        if (previous == null) {
            notifyPersistenceContext();
        }
        return previous;
    }

    @Override
    public void putAll(Map<? extends String, ? extends String> m) {
        super.putAll(m);
        if (!m.isEmpty()) {
            notifyPersistenceContext();
        }
    }

    @Override
    public void remove(String language) {
        remove((Object) language);
    }

    @Override
    public String remove(Object key) {
        final String previous = super.remove(key);
        if (previous != null) {
            notifyPersistenceContext();
        }
        return previous;
    }

    @Override
    public boolean remove(Object key, Object value) {
        final boolean removed = super.remove(key, value);
        if (removed) {
            notifyPersistenceContext();
        }
        return removed;
    }

    @Override
    public void clear() {
        if (!isEmpty()) {
            super.clear();
            notifyPersistenceContext();
        }
    }

    @Override
    public String replace(String key, String value) {
        final String previous = super.replace(key, value);
        if (previous != null) {
            notifyPersistenceContext();
        }
        return previous;
    }

    @Override
    public boolean replace(String key, String oldValue, String newValue) {
        final boolean replaced = super.replace(key, oldValue, newValue);
        if (replaced) {
            notifyPersistenceContext();
        }
        return replaced;
    }

    @Override
    public void replaceAll(BiFunction<? super String, ? super String, ? extends String> function) {
        super.replaceAll(function);
        notifyPersistenceContext();
    }

    @Override
    public String computeIfAbsent(String key, Function<? super String, ? extends String> mappingFunction) {
        final String previous = get(key);
        final String result = super.computeIfAbsent(key, mappingFunction);
        if (previous == null && result != null) {
            notifyPersistenceContext();
        }
        return result;
    }

    @Override
    public String computeIfPresent(String key,
                                   BiFunction<? super String, ? super String, ? extends String> remappingFunction) {
        final boolean hadValue = get(key) != null;
        final String result = super.computeIfPresent(key, remappingFunction);
        if (hadValue) {
            notifyPersistenceContext();
        }
        return result;
    }

    @Override
    public String compute(String key,
                          BiFunction<? super String, ? super String, ? extends String> remappingFunction) {
        final String result = super.compute(key, remappingFunction);
        notifyPersistenceContext();
        return result;
    }

    @Override
    public String merge(String key, String value,
                        BiFunction<? super String, ? super String, ? extends String> remappingFunction) {
        final String result = super.merge(key, value, remappingFunction);
        notifyPersistenceContext();
        return result;
    }

    @Override
    public MultilingualString unwrap() {
        return new MultilingualString(this);
    }
}
