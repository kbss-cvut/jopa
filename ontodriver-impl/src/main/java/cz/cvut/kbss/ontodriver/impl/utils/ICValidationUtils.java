package cz.cvut.kbss.ontodriver.impl.utils;

import java.util.Collection;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;

/**
 * Utility class for validation of integrity constraints.
 * 
 * @author ledvima1
 * 
 */
public abstract class ICValidationUtils {

	private static final Logger LOG = Logger.getLogger(ICValidationUtils.class.getName());

	private ICValidationUtils() {
		// private constructor
	}

	/**
	 * Validate integrity constraints on the specified attribute.
	 * 
	 * @param entity
	 *            Entity to validate
	 * @param primaryKey
	 *            Entity primary key
	 * @param attribute
	 *            The attribute to validate
	 */
	public static void validateIntegrityConstraints(Object entity, Object primaryKey,
			Attribute<?, ?> attribute) {
		if (entity == null || primaryKey == null || attribute == null) {
			throw new NullPointerException("Null passed to validateIntegrityConstraints: entity = "
					+ entity + ", primaryKey = " + primaryKey + ", attribute = " + attribute);
		}
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("CHECKING IC for " + entity + ", attribute=" + attribute.getIRI());
		}
		try {
			Object value = attribute.getJavaField().get(entity);
			Collection<?> set;
			if (value == null) {
				set = Collections.emptySet();
			} else if (attribute.isCollection()) {
				set = (Collection<?>) value;
			} else {
				set = Collections.singleton(value);
			}
			if (LOG.isLoggable(Level.FINER)) {
				LOG.finer("    size=" + set.size());
			}
			for (ParticipationConstraint ic : attribute.getConstraints()) {
				if (LOG.isLoggable(Level.FINER)) {
					LOG.finer("         IC:" + ic.min() + " : " + ic.max());
				}
				if (set.size() < ic.min() || (set.size() > ic.max() && ic.max() >= 0)) {
					throw new IntegrityConstraintViolatedException("Violated min=" + ic.min()
							+ ", max=" + ic.max() + ", for attribute=" + attribute
							+ " of object with id=" + primaryKey);
				}
				// TODO FILLER
			}
		} catch (IllegalAccessException e) {
			throw new OntoDriverInitializationException(e);
		}
	}
}
