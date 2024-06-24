/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.matcher.ElementMatcher;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;

/**
 * Matches only persistent attribute getters/setters.
 * <p>
 * This matcher checks that there exists a persistent field corresponding to the method name that appears to be a
 * getter/setter.
 *
 * @param <T> MethodDescription
 */
public abstract class PersistentPropertyMatcher<T extends MethodDescription> extends ElementMatcher.Junction.ForNonNullValues<T> {

    private final Class<?> parentType;

    public PersistentPropertyMatcher(Class<?> parentType) {this.parentType = Objects.requireNonNull(parentType);}

    @Override
    protected boolean doMatch(T target) {
        final String name = target.getName();
        final Optional<String> fieldName = resolveFieldName(name, target);
        return fieldName.flatMap(this::tryFindingField).map(f -> !EntityPropertiesUtils.isFieldTransient(f))
                        .orElse(false);
    }

    protected abstract Optional<String> resolveFieldName(String methodName, MethodDescription methodDesc);

    protected Optional<Field> tryFindingField(String fieldName) {
        Class<?> type = parentType;
        do {
            try {
                Field f = type.getDeclaredField(fieldName);
                return Optional.of(f);
            } catch (NoSuchFieldException e) {
                type = type.getSuperclass();
            }
        } while (type != null);
        return Optional.empty();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PersistentPropertyMatcher<?> that)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        return Objects.equals(parentType, that.parentType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), parentType);
    }
}
