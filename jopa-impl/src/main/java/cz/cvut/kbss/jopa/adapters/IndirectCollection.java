package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public abstract class IndirectCollection {

	protected final Object owner;
	protected final UnitOfWorkImpl persistenceContext;

	/**
	 * Create new indirect collection from the specified data. </p>
	 * 
	 * The owner is null, the persistence context is not.
	 * 
	 * @param owner
	 *            Owner of the indirect collection
	 * @param persistenceContext
	 *            Persistence context the owner belongs to
	 * @throws NullPointerException
	 *             If the persistence context is null
	 */
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
