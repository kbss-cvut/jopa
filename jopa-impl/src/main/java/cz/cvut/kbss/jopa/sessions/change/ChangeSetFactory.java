package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.sessions.change.UnitOfWorkChangeSetImpl;

public class ChangeSetFactory {

	private ChangeSetFactory() {
		throw new AssertionError();
	}

	/**
	 * Creates change set for the specified UnitOfWork.
	 * 
	 * @return New change set
	 */
	public static UnitOfWorkChangeSet createUoWChangeSet() {
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
	public static ObjectChangeSet createObjectChangeSet(Object original, Object clone, Descriptor descriptor) {
		assert original != null;
		assert descriptor != null;

		return new ObjectChangeSetImpl(original, clone, descriptor.getContext());
	}
}
