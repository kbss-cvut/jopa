package cz.cvut.kbss.jopa.utils;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

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
	public static IRI getPrimaryKey(Object entity, Metamodel metamodel) {
		if (entity == null || metamodel == null) {
			throw new NullPointerException();
		}
		Object fieldValue;
		try {
			final EntityType<?> type = metamodel.entity(entity.getClass());
			final Field idField = type.getIdentifier().getJavaField();
			idField.setAccessible(true);
			fieldValue = idField.get(entity);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException();
		}
		if (fieldValue == null) {
			return null;
		}

		if (fieldValue instanceof String) {
			return IRI.create((String) fieldValue);
		} else if (fieldValue instanceof URI) {
			return IRI.create((URI) fieldValue);
		} else {
			throw new OWLPersistenceException("Unknown identifier type: " + fieldValue.getClass());
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
		if (value == null) {
			throw new NullPointerException();
		}
		if (value instanceof URI) {
			return (URI) value;
		} else if (value instanceof String) {
			return URI.create((String) value);
		} else if (value instanceof IRI) {
			return ((IRI) value).toURI();
		} else if (value instanceof cz.cvut.kbss.jopa.model.IRI) {
			return ((cz.cvut.kbss.jopa.model.IRI) value).toURI();
		} else if (value instanceof URL) {
			try {
				return ((URL) value).toURI();
			} catch (URISyntaxException e) {
				throw new IllegalArgumentException(e);
			}
		} else {
			throw new IllegalArgumentException("The specified " + value
					+ " cannot be tranformed to URI.");
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
		final List<Field> fields = new ArrayList<Field>();
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
}
