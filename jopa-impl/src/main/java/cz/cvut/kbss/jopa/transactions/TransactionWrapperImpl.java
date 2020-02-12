/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

public abstract class TransactionWrapperImpl implements TransactionWrapper {

    private final AbstractEntityManager entityManager;

    private UnitOfWork transactionUOW;

    public TransactionWrapperImpl(AbstractEntityManager entityManger) {
        this.entityManager = entityManger;
    }

    /**
     * Check if there is an active transaction running. Returns the transaction
     * object or null, if there is none.
     *
     * @return Object
     */
    public abstract Object checkForTransaction();

    /**
     * Register the given UnitOfWork with the current transaction.
     *
     * @param uow UnitOfWork
     */
    public abstract void registerUOWWithTransaction(UnitOfWork uow);

    public UnitOfWork getTransactionUOW() {
        return transactionUOW;
    }

    public void setTransactionUOW(UnitOfWork transactionUOW) {
        this.transactionUOW = transactionUOW;
    }

    AbstractEntityManager getEntityManager() {
        return entityManager;
    }
}
