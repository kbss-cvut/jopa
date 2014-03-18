package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.RepositoryID;

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
	 * @param repository
	 *            Repository identifier
	 * @return Object The clone
	 * @throws NullPointerException
	 *             If {@code original} or {@code repository} is {@code null}
	 */
	public Object buildClone(Object original, RepositoryID repository);

	/**
	 * Builds clone of the given object. </p>
	 * 
	 * This method differs from {@link #buildClone(Object, URI)} in that it
	 * accepts another argument which represents the owner of the built clone.
	 * This is useful in situations when we are cloning attributes directly, e.
	 * g. when lazily loading a field value.
	 * 
	 * @param cloneOwner
	 *            The owner of the created clone
	 * @param clonedField
	 *            The field whose value is being cloned
	 * @param original
	 *            The original to clone
	 * @param repository
	 *            Repository identifier
	 * @return The clone
	 * @throws NullPointerException
	 *             If {@code cloneOwner}, {@code original} or {@code contextUri}
	 *             is {@code null}
	 */
	public Object buildClone(Object cloneOwner, Field clonedField, Object original,
			RepositoryID repository);

	/**
	 * Builds clones of all given objects.
	 * 
	 * @param originals
	 *            Map of entities mapped to ontology context they belong to
	 * @return List
	 */
	public List<?> buildClones(Map<?, URI> originals);

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
	public Object mergeChanges(Object original, Object clone, ObjectChangeSet changeSet,
			MergeManager mergeManager);
}
