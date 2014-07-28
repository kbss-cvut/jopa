package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

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
	 * @param descriptor
	 *            Entity descriptor
	 * @return New object change set
	 */
	static ObjectChangeSet createObjectChangeSet(Object original, Object clone,
			Descriptor descriptor) {
		assert original != null;
		assert descriptor != null;

		return new ObjectChangeSetImpl(original, clone, descriptor.getContext());
	}
}
