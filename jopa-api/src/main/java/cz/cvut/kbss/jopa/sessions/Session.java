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

import java.net.URI;

public interface Session {

    /**
     * Acquires UnitOfWork object to perform transaction operations.
     *
     * @return UnitOfWork
     */
    UnitOfWork acquireUnitOfWork();

    /**
     * Release this session and all its children.
     */
    void release();

    /**
     * Remove the given object from the session's live object cache. This is
     * particularly meant for merging deleted objects from transactions.
     *
     * @param object
     *            Object
     * @param context
     *            Entity context URI
     */
    void removeObjectFromCache(Object object, URI context);
}
