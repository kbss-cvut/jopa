package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.RepositoryID;

class ChangeSetFactory {

	private ChangeSetFactory() {
		throw new AssertionError();
	}

	/**
	 * Creates change set for the specified UnitOfWork.
	 * 
	 * @return New change set
	 */
	static UnitOfWorkChangeSet createUoWChangeSet() {
		return new UnitOfWorkChangeSetImpl();
	}

	/**
	 * Creates new change set for the specified original-clone pair.
	 * 
	 * @param original
	 *            Original object
	 * @param clone
	 *            Clone
	 * @param repository
	 *            Repository to which the original belongs
	 * @return New object change set
	 */
	static ObjectChangeSet createObjectChangeSet(Object original, Object clone,
			RepositoryID repository) {
		assert original != null;
		assert repository != null;

		return new ObjectChangeSetImpl(original, clone, repository);
	}
}
