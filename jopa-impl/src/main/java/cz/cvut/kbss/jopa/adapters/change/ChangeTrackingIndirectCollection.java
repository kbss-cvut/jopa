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
package cz.cvut.kbss.jopa.adapters.change;

import cz.cvut.kbss.jopa.adapters.IndirectWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.lang.reflect.Field;

/**
 * Wraps a collection so that calls to modifying operations are intercepted and reported to the persistence context (if
 * necessary).
 *
 * @param <T> Type of the wrapped object
 */
public abstract class ChangeTrackingIndirectCollection<T> implements IndirectWrapper<T> {

    protected final transient Object owner;
    protected final transient Field field;
    protected final transient UnitOfWork persistenceContext;

    /**
     * Create new indirect collection from the specified data.
     * <p>
     * The owner can be null, the persistence context not.
     *
     * @param owner              Owner of the indirect collection
     * @param f                  The field holding this collection
     * @param persistenceContext Persistence context the owner belongs to
     * @throws NullPointerException If the persistence context is null
     */
    protected ChangeTrackingIndirectCollection(Object owner, Field f, UnitOfWork persistenceContext) {
        if (persistenceContext == null) {
            throw new NullPointerException("Null passed in as persistenceContext.");
        }
        this.owner = owner;
        this.field = f;
        this.persistenceContext = persistenceContext;
    }

    protected void persistChange() {
        assert persistenceContext != null;
        if (persistenceContext.isInTransaction() && !persistenceContext.isInCommit()) {
            persistenceContext.attributeChanged(owner, field);
        }
    }
}
