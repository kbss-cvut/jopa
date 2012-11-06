package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public abstract class IndirectCollection {

	protected final Object owner;
	protected final UnitOfWorkImpl persistenceContext;

	protected IndirectCollection(Object owner, UnitOfWorkImpl persistenceContext) {
		if (persistenceContext == null) {
			throw new NullPointerException("Null passed in as persistenceContext.");
		}
		this.owner = owner;
		this.persistenceContext = persistenceContext;
	}

	protected void persistChange() {
		if (persistenceContext.isInTransaction()) {
			persistenceContext.persistChangeInTransaction(owner);
		}
	}
}
