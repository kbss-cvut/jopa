package cz.cvut.kbss.ontodriver_new.model;

import java.io.Serializable;

/**
 * Represents assertion value. </p>
 * 
 * This interface is a base for both property and class assertion values.
 * 
 * @author ledvima1
 * 
 */
public interface Value<T> extends Serializable {

	/**
	 * Gets this value. </p>
	 * 
	 * @return Value of the appropriate type
	 */
	public T getValue();
}
