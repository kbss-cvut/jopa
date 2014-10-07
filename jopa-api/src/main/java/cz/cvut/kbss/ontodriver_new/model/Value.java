package cz.cvut.kbss.ontodriver_new.model;

import java.io.Serializable;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;

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

	/** Represents a null value - empty value */
	public static final Value<Void> NULL_VALUE = new NullValue();

	private final T value;

	private Value() {
		this.value = null;
	}

	public Value(T value) {
		Objects.requireNonNull(value, ErrorUtils.constructNPXMessage("value"));
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

	/**
	 * Returns a Null object for Value. </p>
	 * 
	 * Since Value requires a non-null value, this method returns a predefined
	 * object which represents a null (empty) value.
	 * 
	 * @return Null value
	 * @see {@link http://en.wikipedia.org/wiki/Null_Object_pattern}
	 */
	public static Value<Void> nullValue() {
		return NULL_VALUE;
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

	private static final class NullValue extends Value<Void> {

		private static final long serialVersionUID = -8829030856649294453L;

		private NullValue() {
			super();
		}

		@Override
		public String stringValue() {
			return "null";
		}
	}
}
