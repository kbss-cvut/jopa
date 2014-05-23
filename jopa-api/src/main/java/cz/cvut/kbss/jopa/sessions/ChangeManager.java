package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;

public interface ChangeManager {

	/**
	 * This method does a quick check to find out whether there are any changes
	 * to the clone. It does a object value comparison, i. e. it compares each
	 * value of the clone against the original value and returns true if a
	 * change is found.
	 * 
	 * @param original
	 *            The original object.
	 * @param clone
	 *            The clone, whose changes we are looking for.
	 * @return True if there is a change (at least one) or false, if the values
	 *         are identical.
	 */
	public boolean hasChanges(Object original, Object clone);

	/**
	 * Calculates the changes that happened to the clone object. If there are no
	 * changes, null is returned. The changes are written into the change set
	 * passed in as argument.
	 * 
	 * @param changeSet
	 *            Contains references to the original and clone objects. Into
	 *            this change set the changes should be propagated
	 * @return {@code true} if there were any changes, {@code false} otherwise
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 *             If {@code changeSet} is {@code null}
	 */
	public boolean calculateChanges(ObjectChangeSet changeSet) throws IllegalAccessException,
			IllegalArgumentException, OWLInferredAttributeModifiedException;

}
