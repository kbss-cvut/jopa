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
		return (state != null ? state.toString() : "null");
	}
}
