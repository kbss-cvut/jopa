/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Constructor;
import java.security.PrivilegedExceptionAction;

/**
 * Builds new instance with privileged access using the specified constructor.
 */
public class PrivilegedInstanceCreator implements PrivilegedExceptionAction<Object> {

    private final Constructor<?> constructor;
    private final Object[] params;

    PrivilegedInstanceCreator(Constructor<?> constructor) {
        this.constructor = constructor;
        this.params = null;
    }

    PrivilegedInstanceCreator(Constructor<?> constructor, Object... params) {
        this.constructor = constructor;
        this.params = params;
    }

    @Override
    public Object run() throws Exception {
        return constructor.newInstance(params);
    }

}
