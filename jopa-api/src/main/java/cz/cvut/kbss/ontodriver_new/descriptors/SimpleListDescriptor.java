package cz.cvut.kbss.ontodriver_new.descriptors;


/**
 * Represents a simple LIPS-style sequence, which is basically a singly-linked list. </p>
 * 
 * Each node in the list is subject to a statement referencing the next node.
 * @author kidney
 *
 */
public interface SimpleListDescriptor extends ListDescriptor {
	
	// We are just exporting the API from BaseListDesciptor, as ReferencedListDescriptor is not a SimpleListDescriptor,
	// but they share the basic common methods.
	// This way, we can declare the methods only once, but the list descriptor have each their own hierarchy.

}