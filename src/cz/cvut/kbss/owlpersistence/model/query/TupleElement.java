package cz.cvut.kbss.owlpersistence.model.query;

/**
 * The TupleElement interface defines an element that is returned in a query
 * result tuple.
 * 
 * @param <X>
 *            the type of the element
 */
public interface TupleElement<X> {
	/**
	 * Return the runtime Java type of the tuple element.
	 * 
	 * @return the runtime Java type of the tuple element
	 */
	Class<? extends X> getJavaType();

	/**
	 * Return the alias assigned to the tuple element or null, if no alias has
	 * been assigned.
	 * 
	 * @return alias
	 */
	String getAlias();
}
