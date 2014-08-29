package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collection;
import java.util.Date;

import org.openrdf.model.Literal;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.XMLSchema;

/**
 * Utility methods for the Sesame driver.
 * 
 * @author ledvima1
 * 
 */
final class SesameUtils {

	private SesameUtils() {
		// Private constructor
	}

	/**
	 * Gets value of the specified data property literal as the corresponding
	 * Java object. Primitives are returned boxed.
	 * 
	 * @param literal
	 *            DataProperty value
	 * @return Java value corresponding to the XML Schema datatype of the
	 *         literal
	 * @throws IllegalArgumentException
	 *             If literal's datatype is not supported
	 */
	static Object getDataPropertyValue(Literal literal) {
		assert literal != null;

		final URI datatype = literal.getDatatype();
		if (datatype == null || datatype.equals(XMLSchema.STRING)
				|| datatype.equals(XMLSchema.NORMALIZEDSTRING)) {
			return literal.stringValue();
		} else if (datatype.equals(XMLSchema.INT) || datatype.equals(XMLSchema.UNSIGNED_INT)) {
			return Integer.valueOf(literal.intValue());
		} else if (datatype.equals(XMLSchema.INTEGER)
				|| datatype.equals(XMLSchema.POSITIVE_INTEGER)
				|| datatype.equals(XMLSchema.NON_NEGATIVE_INTEGER)
				|| datatype.equals(XMLSchema.NEGATIVE_INTEGER)
				|| datatype.equals(XMLSchema.NON_POSITIVE_INTEGER)) {
			return literal.intValue();
		} else if (datatype.equals(XMLSchema.BOOLEAN)) {
			return literal.booleanValue();
		} else if (datatype.equals(XMLSchema.LONG) || datatype.equals(XMLSchema.UNSIGNED_LONG)) {
			return Long.valueOf(literal.longValue());
		} else if (datatype.equals(XMLSchema.DECIMAL)) {
			return literal.decimalValue();
		} else if (datatype.equals(XMLSchema.DOUBLE)) {
			return Double.valueOf(literal.doubleValue());
		} else if (datatype.equals(XMLSchema.SHORT) || datatype.equals(XMLSchema.UNSIGNED_SHORT)) {
			return Short.valueOf(literal.shortValue());
		} else if (datatype.equals(XMLSchema.BYTE) || datatype.equals(XMLSchema.UNSIGNED_BYTE)) {
			return Byte.valueOf(literal.byteValue());
		} else if (datatype.equals(XMLSchema.DATE) || datatype.equals(XMLSchema.DATETIME)) {
			return literal.calendarValue().toGregorianCalendar().getTime();
		} else {
			throw new IllegalArgumentException("Unsupported datatype " + datatype);
		}
	}

	/**
	 * Creates Sesame literal from the specified value, which can be used as
	 * data property object.
	 * 
	 * @param value
	 *            The value to transform
	 * @param language
	 *            Language to add to string literals
	 * @param vf
	 *            Sesame value factory
	 * @return Sesame Literal
	 * @throws IllegalArgumentException
	 *             If the type of the value is not supported
	 */
	static Literal createDataPropertyLiteral(Object value, String language, ValueFactory vf) {
		assert value != null;

		if (value instanceof Integer) {
			return vf.createLiteral((Integer) value);
		} else if (value instanceof String) {
			return vf.createLiteral((String) value, language);
		} else if (value instanceof Byte) {
			return vf.createLiteral((Byte) value);
		} else if (value instanceof Short) {
			return vf.createLiteral((Short) value);
		} else if (value instanceof Boolean) {
			return vf.createLiteral((Boolean) value);
		} else if (value instanceof Double) {
			return vf.createLiteral((Double) value);
		} else if (value instanceof Long) {
			return vf.createLiteral((Long) value);
		} else if (value instanceof Date) {
			return vf.createLiteral((Date) value);
		} else {
			throw new IllegalArgumentException("Unsupported literal type " + value.getClass());
		}
	}

	/**
	 * Returns the specified collection as an array so that it can be used as
	 * varargs.
	 * 
	 * @param col
	 *            The collection to transform
	 * @return Array representing the collection
	 */
	static URI[] varargs(Collection<URI> col) {
		assert col != null;

		return col.toArray(new URI[col.size()]);
	}

	/**
	 * Constructs a Sesame URI from the specified java.net.URI.
	 * 
	 * @param javaUri
	 *            The uri to convert
	 * @param factory
	 *            ValueFactory used for the conversion
	 * @return Sesame URI
	 */
	static URI toSesameUri(java.net.URI javaUri, ValueFactory factory) {
		return (javaUri != null ? factory.createURI(javaUri.toString()) : null);
	}
}
