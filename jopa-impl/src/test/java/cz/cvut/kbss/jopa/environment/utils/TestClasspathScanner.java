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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.loaders.DefaultClasspathScanner;

/**
 * Classpath scanner that skips classes annotated with {@link TestLocal}.
 */
public class TestClasspathScanner extends DefaultClasspathScanner {

    @Override
    protected void processClass(String className) {
        try {
            final Class<?> cls = Class.forName(className, true, classLoader);
            if (!cls.isAnnotationPresent(TestLocal.class)) {
                listeners.forEach(listener -> listener.accept(cls));
            }
        } catch (ClassNotFoundException e) {
            throw new OWLPersistenceException("Unexpected ClassNotFoundException when scanning for entities.", e);
        }
    }
}
