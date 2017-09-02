/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import static cz.cvut.kbss.ontodriver.sesame.TransactionState.ABORTED;
import static cz.cvut.kbss.ontodriver.sesame.TransactionState.ACTIVE;
import static cz.cvut.kbss.ontodriver.sesame.TransactionState.COMMITTED;
import static cz.cvut.kbss.ontodriver.sesame.TransactionState.FAILED;
import static cz.cvut.kbss.ontodriver.sesame.TransactionState.PARTIALLY_COMMITTED;

public final class Transaction {

    private TransactionState state;

    public void begin() {
        if (state != null && state != ABORTED && state != COMMITTED) {
            throw new IllegalStateException("Cannot begin transaction. Current state is " + state);
        }
        this.state = ACTIVE;
    }

    public void commit() {
        if (state != ACTIVE) {
            throw new IllegalStateException("Cannot commit transaction. Current state is " + state);
        }
        this.state = PARTIALLY_COMMITTED;
    }

    public void afterCommit() {
        if (state != PARTIALLY_COMMITTED) {
            throw new IllegalStateException("Cannot finish commit. Current state is " + state);
        }
        this.state = COMMITTED;
    }

    public void rollback() {
        if (state != ACTIVE && state != PARTIALLY_COMMITTED && state != FAILED) {
            throw new IllegalStateException("Cannot rollback transaction. Current state is "
                    + state);
        }
        this.state = FAILED;
    }

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
        if (state != other.state)
            return false;
        return true;
    }

    @Override
    public String toString() {
        return state != null ? state.toString() : "null";
    }
}
