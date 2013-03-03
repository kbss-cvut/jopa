package cz.cvut.kbss.jopa.utils;

import java.net.URI;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
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
			fieldValue = type.getIdentifier().getJavaField().get(entity);
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
			throw new OWLPersistenceException("Unknown identifier type: "
					+ fieldValue.getClass());
		}
	}
}
