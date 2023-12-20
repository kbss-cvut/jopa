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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.sessions.ConfigurationHolder;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;

public interface AbstractEntityManager extends EntityManager, ConfigurationHolder {

    boolean isLoaded(final Object object, final String attributeName);

    boolean isLoaded(final Object object);

    /**
     * Return the UnitOfWork that holds the current persistence context.
     *
     * @return UnitOfWork
     */
    UnitOfWork getCurrentPersistenceContext();

    /**
     * Remove the current persistence context UnitOfWork.
     */
    void removeCurrentPersistenceContext();

    /**
     * Let the managing server session know that a transaction has been started.
     *
     * @param t The entity transaction that was started.
     */
    @NonJPA
    void transactionStarted(EntityTransaction t);

    /**
     * Let the managing server session know that a transaction has finished successfully.
     *
     * @param t The committed entity transaction.
     */
    @NonJPA
    void transactionFinished(EntityTransaction t);
}
