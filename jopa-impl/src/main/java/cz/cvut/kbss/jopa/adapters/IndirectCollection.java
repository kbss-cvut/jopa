/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.lang.reflect.Field;

/**
 * Wraps a collection so that calls to modifying operations are intercepted and reported to the persistence context (if necessary).
 * @param <T> Type of the wrapped object
 */
public abstract class IndirectCollection<T> implements IndirectWrapper {

    protected final transient Object owner;
    protected final transient Field field;
    protected final transient UnitOfWorkImpl persistenceContext;

    protected IndirectCollection() {
        owner = null;
        field = null;
        persistenceContext = null;
    }

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
    protected IndirectCollection(Object owner, Field f, UnitOfWorkImpl persistenceContext) {
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

    /**
     * The returned type is determined by the instance type parameter.
     *
     * @return The collection wrapped in this indirect collection
     */
    @Override
    public abstract T unwrap();
}
