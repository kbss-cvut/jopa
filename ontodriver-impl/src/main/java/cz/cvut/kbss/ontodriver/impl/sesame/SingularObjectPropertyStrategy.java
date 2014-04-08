package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collections;
import java.util.logging.Level;

import org.openrdf.model.Statement;
import org.openrdf.model.URI;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Strategy for singular object property values. </p>
 * 
 * I. e. references to other entities, stored as object property values.
 * 
 * @author ledvima1
 * 
 */
class SingularObjectPropertyStrategy extends AttributeStrategy {

	protected SingularObjectPropertyStrategy(SesameModuleInternal internal, SubjectModels models) {
		super(internal, models);
	}

	@Override
	<T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad)
			throws IllegalArgumentException, IllegalAccessException, OntoDriverException {
		if (!alwaysLoad && att.getFetchType() == FetchType.LAZY) {
			return;
		}
		loadObjectProperty(entity, uri, att);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Fetched property '" + att.getIRI() + "' into field " + att.getJavaField()
					+ "' of object " + uri);
		}
	}

	@Override
	<T> void save(URI primaryKey, Attribute<?, ?> att, Object value, URI context, boolean removeOld)
			throws OntoDriverException {
		final URI attUri = getAddressAsSesameUri(att.getIRI());
		saveObjectProperty(primaryKey, attUri, value, context, removeOld);
	}

	/**
	 * Loads value of the specified object property for the specified entity
	 * instance.
	 * 
	 * @param instance
	 *            Entity instance
	 * @param uri
	 *            Entity primary key
	 * @param property
	 *            Attribute representing the object property
	 * @throws OntoDriverException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 */
	private <T> void loadObjectProperty(T instance, URI uri, Attribute<?, ?> property)
			throws OntoDriverException, IllegalArgumentException, IllegalAccessException {
		final URI propertyUri = getAddressAsSesameUri(property.getIRI());
		URI objectUri = getObjectPropertyValue(uri, propertyUri, property.isInferred(), true);
		if (objectUri == null) {
			if (LOG.isLoggable(Level.FINER)) {
				LOG.finer("Value of object property " + property.getIRI()
						+ " not found or is not a resource.");
			}
			return;
		}
		final Object value = getJavaInstanceForSubject(property.getJavaType(), objectUri);
		if (value != null) {
			property.getJavaField().set(instance, value);
		}
	}

	private void saveObjectProperty(URI subject, URI property, Object value, URI context,
			boolean removeOld) throws OntoDriverException {
		if (removeOld) {
			removeOldObjectPropertyValues(subject, property, context);
		}
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("setObjectProperty '" + property + "' of " + subject + " to " + value);
		}
		if (value != null) {
			addIndividualsForReferencedEntities(Collections.singletonList(value), context);
			final URI uri = getIdentifier(value);
			assert uri != null;
			final Statement stmt = valueFactory.createStatement(subject, property, uri);
			addStatement(stmt, context);
		}
	}
}
