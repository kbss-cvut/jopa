package cz.cvut.kbss.ontodriver_new.model;

import java.io.Serializable;

/**
 * Represents assertion value. </p>
 * 
 * This class is a base for both property and class assertion values.
 * 
 * @author ledvima1
 * 
 */
public class Value<T> implements Serializable {

	private static final long serialVersionUID = -1220526093003250055L;

	private final T value;

	public Value(T value) {
		this.value = value;
	}

	/**
	 * Gets this value. </p>
	 * 
	 * @return Value of the appropriate type
	 */
	public T getValue() {
		return value;
	}

	/**
	 * Gets this value as string. </p>
	 * 
	 * @return Value as string
	 */
	public String stringValue() {
		return (value != null ? value.toString() : "");
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Value<?> other = (Value<?>) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return value.toString();
	}
}
