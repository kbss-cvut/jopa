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
package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.model.metamodel.AnnotatedAccessor;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistentPropertyMatcher;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.description.type.TypeDescription;

import java.beans.Introspector;
import java.util.Optional;

/**
 * Matches only persistent field getters.
 * <p>
 * This matcher checks that there exists a persistent field corresponding to the method name that appears to be a
 * getter. {@code get, is, has} prefixes are supported.
 *
 * @param <T> MethodDescription
 */
public class PersistentPropertyGetterMatcher<T extends MethodDescription> extends PersistentPropertyMatcher<T> {

    public PersistentPropertyGetterMatcher(Class<?> parentType) {
        super(parentType);
    }

    @Override
    protected boolean doMatch(T target) {
        final String name = target.getName();
        final Optional<String> fieldName = resolveFieldName(name, target);
        return fieldName.flatMap(this::tryFindingField)
                        // Field must not be transient, and it must not be an identifier (no need generating getter interceptor
                        // for identifier)
                        .map(f -> !EntityPropertiesUtils.isFieldTransient(f))
                        .orElse(false);
    }

    @Override
    protected Optional<String> resolveFieldName(String methodName, MethodDescription methodDesc) {
        assert methodDesc.getParameters().isEmpty();
        final TypeDescription.Generic returnType = methodDesc.getReturnType();
        if (methodName.startsWith(AnnotatedAccessor.GET_PREFIX)) {
            return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.GET_PREFIX.length())));
        } else if (methodName.startsWith(AnnotatedAccessor.IS_PREFIX) && (returnType.represents(Boolean.class))) {
            return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.IS_PREFIX.length())));
        } else if (methodName.startsWith(AnnotatedAccessor.HAS_PREFIX) && methodDesc.getReturnType()
                                                                                    .represents(Boolean.class)) {
            return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.HAS_PREFIX.length())));
        } else {
            return Optional.empty();
        }
    }
}

