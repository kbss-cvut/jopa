package cz.cvut.kbss.ontodriver_new.model;

/**
 * Represents assertion value. </p>
 * 
 * This interface is a base for both property and class assertion values.
 * 
 * @author ledvima1
 * 
 */
public interface Value<T> {

	/**
	 * Gets this value. </p>
	 * 
	 * @return Value of the appropriate type
	 */
	public T getValue();
}
