package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.util.Date;

import org.openrdf.model.Literal;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.XMLSchema;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Utility methods for the Sesame storage module and connector.
 * 
 * @author ledvima1
 * 
 */
abstract class SesameUtils {

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
			return literal.integerValue();
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
			return literal.calendarValue();
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
		} else if (value instanceof Boolean) {
			return vf.createLiteral((Boolean) value);
		} else if (value instanceof String) {
			return vf.createLiteral((String) value, language);
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
	 * Returns true if the specified property is present among the attributes of
	 * the specified entity type.
	 * 
	 * @param property
	 *            Property URI
	 * @param entityType
	 *            Entity type
	 * @return {@code true} if the entity type defines attribute using the
	 *         specified property, {@code false} otherwise
	 */
	static boolean isEntityAttribute(URI property, EntityType<?> entityType) {
		assert property != null : "argument property is null";
		assert entityType != null : "argument entityType is null";

		final String strProperty = property.stringValue();
		for (Attribute<?, ?> att : entityType.getAttributes()) {
			if (strProperty.equals(att.getIRI().toString())) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Sets the identifier field value on the specified entity instance.
	 * 
	 * @param entityType
	 *            Entity type
	 * @param instance
	 *            The instance to set identifier on
	 * @param uri
	 *            Sesame URI, identifier value
	 * @throws OntoDriverException
	 *             If the identifier cannot be set
	 */
	static <T> void setEntityIdentifier(EntityType<T> entityType, T instance, URI uri)
			throws OntoDriverException {
		assert entityType != null : "argument entityType is null";
		assert instance != null : "argument instance is null";

		final Field idField = entityType.getIdentifier().getJavaField();
		try {
			if (uri == null) {
				idField.set(instance, null);
			} else if (String.class.equals(idField.getType())) {
				idField.set(instance, uri.toString());
			} else if (java.net.URI.class.equals(idField.getType())) {
				idField.set(instance, java.net.URI.create(uri.stringValue()));
			} else {
				throw new IllegalStateException("Unsuppported primary key type "
						+ idField.getType());
			}
		} catch (IllegalAccessException | IllegalArgumentException e) {
			throw new OntoDriverException("Exception caught when setting identifier.", e);
		}
	}
}
