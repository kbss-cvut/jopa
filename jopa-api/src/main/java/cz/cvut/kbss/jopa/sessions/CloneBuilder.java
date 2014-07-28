package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

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
	 * @param descriptor
	 *            Entity descriptor
	 * @return Object The clone
	 * @throws NullPointerException
	 *             If {@code original} or {@code repository} is {@code null}
	 */
	public Object buildClone(Object original, Descriptor descriptor);

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
	 * @param descriptor
	 *            Entity descriptor
	 * @return The clone
	 * @throws NullPointerException
	 *             If {@code cloneOwner}, {@code original} or {@code contextUri}
	 *             is {@code null}
	 */
	public Object buildClone(Object cloneOwner, Field clonedField, Object original,
			Descriptor descriptor);

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
