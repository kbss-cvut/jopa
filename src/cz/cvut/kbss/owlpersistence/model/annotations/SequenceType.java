package cz.cvut.kbss.owlpersistence.model.annotations;

/**
 * Defines types of sequence modeling in OWL.
 * 
 * @author kremen
 */
public enum SequenceType {
	/**
	 * Used for simple (nonreferenced) sequences.
	 * 
	 * This means that elements of the sequence are unique to the sequence owner
	 * and are NOT shared with other sequences.
	 * 
	 * TODO example
	 */
	simple,

	/**
	 * Used for referenced sequences. This case is more general, but sequence
	 * representation requires more space (linear in the original size)
	 * 
	 * This means that elements of the sequence are not unique to the sequence
	 * owner. Thus these elements might be referenced by other sequences.
	 * 
	 * TODO example
	 */
	referenced;
}
