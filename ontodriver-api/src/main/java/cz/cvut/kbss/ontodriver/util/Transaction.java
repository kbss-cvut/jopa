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
package cz.cvut.kbss.ontodriver.util;


import static cz.cvut.kbss.ontodriver.util.TransactionState.*;

public final class Transaction {

    private TransactionState state;

    /**
     * Begins a transaction.
     */
    public void begin() {
        if (state != null && state != ABORTED && state != COMMITTED) {
            throw new IllegalStateException("Cannot begin transaction. Current state is " + state);
        }
        this.state = ACTIVE;
    }

    /**
     * Marks the beginning of transaction commit.
     *
     * @see #afterCommit()
     */
    public void commit() {
        verifyActive();
        this.state = PARTIALLY_COMMITTED;
    }

    /**
     * Marks successful completion of transaction commit.
     *
     * @see #commit()
     */
    public void afterCommit() {
        if (state != PARTIALLY_COMMITTED) {
            throw new IllegalStateException("Cannot finish commit. Current state is " + state);
        }
        this.state = COMMITTED;
    }

    /**
     * Commences transaction rollback.
     *
     * @see #afterRollback()
     */
    public void rollback() {
        if (state != ACTIVE && state != PARTIALLY_COMMITTED && state != FAILED) {
            throw new IllegalStateException("Cannot rollback transaction. Current state is "
                    + state);
        }
        this.state = FAILED;
    }

    /**
     * Marks successful finish of transaction rollback.
     *
     * @see #rollback()
     */
    public void afterRollback() {
        if (state != FAILED) {
            throw new IllegalStateException("Cannot finish rollback. Current state is " + state);
        }
        this.state = ABORTED;
    }

    public boolean isActive() {
        return state == ACTIVE;
    }

    public TransactionState getState() {
        return state;
    }

    /**
     * Verifies that the transaction is active.
     *
     * @throws IllegalStateException When transaction is not active
     */
    public void verifyActive() {
        if (!isActive()) {
            throw new IllegalStateException("Transaction is not active. Current state is " + state);
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((state == null) ? 0 : state.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Transaction other = (Transaction) obj;
        return state == other.state;
    }

    @Override
    public String toString() {
        return state != null ? state.toString() : "null";
    }
}
