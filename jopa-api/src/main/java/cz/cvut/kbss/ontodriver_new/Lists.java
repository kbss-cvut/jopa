package cz.cvut.kbss.ontodriver_new;

import java.util.Collection;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

/**
 * This interface is used to work with ontology sequences. </p>
 * 
 * Currently, two kinds of sequences are supported:
 * <ul>
 * <li><b>Simple lists</b> - simple LISP-style singly-linked lists, where each
 * node is the subject of a statement pointing to the next node,</li>
 * <li><b>Referenced lists</b> - lists, where each node points to the next
 * element and also to its contents.</li>
 * </ul>
 * 
 * @author kidney
 * 
 */
public interface Lists {

	/**
	 * Loads simple list specified by the descriptor. </p>
	 * 
	 * The returned axioms should be iterable in the same order as they were put
	 * into the sequence in the ontology.
	 * 
	 * @param descriptor
	 *            Describes the list's properties as well as the context from
	 *            which the list should be loaded.
	 * @return Axioms matching the specified list descriptor
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public Collection<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor descriptor)
			throws OntoDriverException;

	/**
	 * Persists simple list values specified by the descriptor. </p>
	 * 
	 * The sequence is persisted in the order in which it appears in the
	 * descriptor.
	 * 
	 * @param descriptor
	 *            List values descriptor
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public void persistSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException;

	/**
	 * Updates simple list based on values specified in the descriptor. </p>
	 * 
	 * It is up to the driver implementation to decide whether the old list will
	 * be removed and the new values inserted or whether a merge will be
	 * performed.
	 * 
	 * @param descriptor
	 *            List values descriptor
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public void updateSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException;

	/**
	 * Loads referenced list specified by the descriptor. </p>
	 * 
	 * The returned axioms should be iterable in the same order as they were put
	 * into the sequence in the ontology.
	 * 
	 * @param descriptor
	 *            Describes list's properties, including node content assertion.
	 *            Also may specify context from which the list should be loaded
	 * @return Axioms matching the specified list descriptor
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public Collection<Axiom<NamedResource>> loadReferencedList(ReferencedListDescriptor descriptor)
			throws OntoDriverException;

	/**
	 * Persists referenced list values specified by the descriptor. </p>
	 * 
	 * The sequence is persisted in the order in which it appears in the
	 * descriptor.
	 * 
	 * @param descriptor
	 *            List values descriptor
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public void persistReferencedList(ReferencedListValueDescriptor descriptor)
			throws OntoDriverException;

	/**
	 * Updates referenced list based on the values in the specified list
	 * descriptor. </p>
	 * 
	 * It is up to the driver implementation whether the update will be realized
	 * by removing the old list and persisting the new values, or by merging the
	 * new list into the old one.
	 * 
	 * @param descriptor
	 *            List values descriptor
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws IllegalStateException
	 *             If called on a closed connection
	 */
	public void updateReferencedList(ReferencedListValueDescriptor descriptor)
			throws OntoDriverException;
}
