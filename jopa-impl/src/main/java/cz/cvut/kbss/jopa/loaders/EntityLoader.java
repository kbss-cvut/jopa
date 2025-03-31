/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.util.NonEntity;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Registers entity classes (classes annotated with {@link OWLClass}) discovered during classpath processing.
 */
class EntityLoader implements Consumer<Class<?>> {

    private final Set<Class<?>> entities = new HashSet<>();

    public Set<Class<?>> getEntities() {
        return entities;
    }

    @Override
    public void accept(Class<?> cls) {
        if (cls.getAnnotation(OWLClass.class) != null  && cls.getAnnotation(NonEntity.class) == null) {
            entities.add(cls);
        }
    }
}
