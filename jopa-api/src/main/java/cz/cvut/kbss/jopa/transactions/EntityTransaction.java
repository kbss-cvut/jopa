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
package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.model.EntityManager;

/**
 * Interface used to control transactions on resource-local entity managers.
 * <p>
 * The {@link EntityManager#getTransaction()} method returns the {@code EntityTransaction} interface.
 */
public interface EntityTransaction {

    /**
     * Start a resource transaction.
     *
     * @throws IllegalStateException if {@code isActive()} is true
     */
    void begin();

    /**
     * Commit the current resource transaction, writing any unflushed changes to the database.
     *
     * @throws IllegalStateException                          if {@code isActive()} is false
     * @throws cz.cvut.kbss.jopa.exceptions.RollbackException if the commit fails
     */
    void commit();

    /**
     * Roll back the current resource transaction.
     *
     * @throws IllegalStateException                                if {@code isActive()} is false
     * @throws cz.cvut.kbss.jopa.exceptions.OWLPersistenceException if an unexpected error condition is encountered
     */
    void rollback();

    /**
     * Mark the current resource transaction so that the only possible outcome of the transaction is for the transaction
     * to be rolled back.
     *
     * @throws IllegalStateException if {@code isActive()} is false
     */
    void setRollbackOnly();

    /**
     * Determine whether the current resource transaction has been marked for rollback.
     *
     * @return boolean indicating whether the transaction has been marked for rollback
     * @throws IllegalStateException if {@code isActive()} is false
     */
    boolean isRollbackOnly();

    /**
     * Indicate whether a resource transaction is in progress.
     *
     * @return boolean indicating whether transaction is in progress
     * @throws cz.cvut.kbss.jopa.exceptions.OWLPersistenceException if an unexpected error condition is encountered
     */
    boolean isActive();
}
