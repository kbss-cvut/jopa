package cz.cvut.kbss.jopa.sessions;

import java.util.List;

/**
 * Objects of this interface are responsible for building clones for UnitOfWork
 * transactions.
 * 
 * @author kidney
 * 
 */
public interface CloneBuilder {

	/**
	 * Builds clone of the given object.
	 * 
	 * @param original
	 *            Object
	 * @return Object The clone
	 */
	public Object buildClone(Object original);

	/**
	 * Builds clones of all given objects.
	 * 
	 * @param originals
	 *            List
	 * @return List
	 */
	public List<?> buildClones(List<?> originals);

	/**
	 * Creates ObjectChangeSet for the given object.
	 * 
	 * @param original
	 *            The original object.
	 * @param clone
	 *            The clone of the original object containing the changed
	 *            values.
	 * @param changeSet
	 *            UnitOfWorkChangeSet (The owner of created changeSet)
	 * @return ObjectChangeSet
	 */
	public ObjectChangeSet createObjectChangeSet(Object original, Object clone,
			UnitOfWorkChangeSet changeSet);

	/**
	 * Resets the clone builder. Especially resets the visited objects cache to
	 * make sure all the clones are built from scratch and are not affected by
	 * the previously built ones.
	 */
	public void reset();

	/**
	 * Merges the changes on clone into the original object
	 * 
	 * @param original
	 *            Object
	 * @param clone
	 *            Object
	 * @param changeSet
	 *            ObjectChangeSet
	 * @param mergeManager
	 *            MergeManager
	 * @return
	 */
	public Object mergeChanges(Object original, Object clone,
			ObjectChangeSet changeSet, MergeManager mergeManager);

}
