package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.UnassignableIdentifierException;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.*;

/**
 * Utility class for entity properties.
 * 
 * @author kidney
 * 
 */
public class EntityPropertiesUtils {

	/**
	 * Private constructor
	 */
	private EntityPropertiesUtils() {
	}

	/**
	 * Extracts primary key from the specified {@code entity} and returns it.
	 * </p>
	 * 
	 * @param entity
	 *            The entity to extract primary key from
	 * @param metamodel
	 *            Metamodel
	 * @return IRI of the entity or null if it is not set
	 * @throws NullPointerException
	 *             If {@code entity} or {@code metamodel} is null
	 * @throws OWLPersistenceException
	 *             If {@code entity} is not an entity or if the identifier is of
	 *             an unknown type
	 */
	public static Object getPrimaryKey(Object entity, Metamodel metamodel) {
		if (entity == null || metamodel == null) {
			throw new NullPointerException();
		}
		Object fieldValue;
		try {
			final EntityType<?> type = metamodel.entity(entity.getClass());
			final Field idField = type.getIdentifier().getJavaField();
			idField.setAccessible(true);
			fieldValue = idField.get(entity);
            return fieldValue;
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException();
		}
//		if (fieldValue == null) {
//			return null;
//		}

//		if (fieldValue instanceof String) {
//			return IRI.create((String) fieldValue);
//		} else if (fieldValue instanceof URI) {
//			return IRI.create((URI) fieldValue);
//		} else {
//			throw new OWLPersistenceException("Unknown identifier type: " + fieldValue.getClass());
//		}
	}

	/**
	 * Extracts entity's primary key according to the specified entity type.
	 * 
	 * @param entity
	 *            Entity
	 * @param et
	 *            Entity type
	 * @return Primary key, possibly null
	 */
	public static <T> URI getPrimaryKey(T entity, EntityType<?> et) {
		final Field idField = et.getIdentifier().getJavaField();
		if (!idField.isAccessible()) {
			idField.setAccessible(true);
		}
		try {
			final Object id = idField.get(entity);
			if (id == null) {
				return null;
			}
			return getValueAsURI(id);
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new OWLPersistenceException("Unable to extract entity identifier.", e);
		}
	}

	public static <T> void setPrimaryKey(Object primaryKey, T entity, EntityType<T> et) {
		final Identifier id = et.getIdentifier();
		final Field idField = id.getJavaField();
		if (!idField.getType().isAssignableFrom(primaryKey.getClass())) {
			throw new UnassignableIdentifierException("Cannot assign identifier of type "
					+ primaryKey + " to field of type " + idField.getType());
		}
		if (!idField.isAccessible()) {
			idField.setAccessible(true);
		}
		try {
			idField.set(entity, primaryKey);
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new OWLPersistenceException("Unable to set entity primary key.", e);
		}
	}

	/**
	 * Transforms the specified value to URI (if possible). </p>
	 * 
	 * @param value
	 *            The value to transform
	 * @return {@code URI}
	 * @throws NullPointerException
	 *             If {@code value} is {@code null}
	 * @throws IllegalArgumentException
	 *             If {@code value} cannot be transformed to URI
	 */
	public static URI getValueAsURI(Object value) {
		Objects.requireNonNull(value, ErrorUtils.constructNPXMessage("value"));

		if (value instanceof URI) {
			return (URI) value;
		} else {
			return URI.create(value.toString());
		}
	}

	/**
	 * Gets all instance fields of the specified class, including inherited
	 * ones. </p>
	 * 
	 * @param cls
	 *            The class to search
	 * @return List of declared fields
	 */
	public static List<Field> getAllFields(Class<?> cls) {
		final List<Field> fields = new ArrayList<>();
		fields.addAll(Arrays.asList(cls.getDeclaredFields()));
		Class<?> tmp = cls.getSuperclass();
		while (tmp != null) {
			fields.addAll(Arrays.asList(tmp.getDeclaredFields()));
			tmp = tmp.getSuperclass();
		}
		Iterator<Field> it = fields.iterator();
		while (it.hasNext()) {
			Field f = it.next();
			if (Modifier.isStatic(f.getModifiers())) {
				it.remove();
			}
		}
		return fields;
	}

	/**
	 * Verifies, that the primary key (identifier) of the specified instance is
	 * generated. </p>
	 * 
	 * If not, an exception is thrown.
	 * 
	 * @param instance
	 *            The instance to verify
	 * @param entityType
	 *            Entity type of the instance, as specified by metamodel
	 * @throws PrimaryKeyNotSetException
	 *             If the identifier is not generated
	 */
	public static void verifyIdentifierIsGenerated(Object instance, EntityType<?> entityType) {
		if (!entityType.getIdentifier().isGenerated()) {
			throw new PrimaryKeyNotSetException("The id for entity " + instance
					+ " is null and it is not specified as \'generated\' ");
		}
	}
}
