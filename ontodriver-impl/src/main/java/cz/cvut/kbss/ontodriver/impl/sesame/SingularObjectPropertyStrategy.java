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

	protected SingularObjectPropertyStrategy(SesameModuleInternal internal, SubjectModels<?> models) {
		super(internal, models);
	}

	@Override
	<T> void load(Attribute<?, ?> att, boolean alwaysLoad) throws IllegalArgumentException,
			IllegalAccessException, OntoDriverException {
		if (!alwaysLoad && att.getFetchType() == FetchType.LAZY) {
			return;
		}
		loadObjectProperty(att);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Fetched property '" + att.getIRI() + "' into field " + att.getJavaField()
					+ "' of object " + models.primaryKey);
		}
	}

	@Override
	<T> void save(Attribute<?, ?> att, Object value, boolean removeOld) throws OntoDriverException {
		final URI attUri = getAddressAsSesameUri(att.getIRI());
		saveObjectProperty(attUri, value, att, removeOld);
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
	private <T> void loadObjectProperty(Attribute<?, ?> property) throws OntoDriverException,
			IllegalArgumentException, IllegalAccessException {
		final URI propertyUri = getAddressAsSesameUri(property.getIRI());
		final URI ctx = models.getFieldContext(property);
		URI objectUri = getObjectPropertyValue(models.primaryKey, propertyUri,
				property.isInferred(), ctx);
		if (objectUri == null) {
			if (LOG.isLoggable(Level.FINER)) {
				LOG.finer("Value of object property " + property.getIRI()
						+ " not found or is not a resource.");
			}
			return;
		}
		final Object value = getJavaInstanceForSubject(property.getJavaType(), objectUri, ctx);
		if (value != null) {
			property.getJavaField().set(models.entity, value);
		}
	}

	private void saveObjectProperty(URI property, Object value, Attribute<?, ?> att,
			boolean removeOld) throws OntoDriverException {
		final URI ctx = models.getFieldContext(att);
		if (removeOld) {
			removeOldObjectPropertyValues(models.primaryKey, property, ctx);
		}
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("setObjectProperty '" + property + "' of " + models.primaryKey + " to "
					+ value);
		}
		if (value != null) {
			addIndividualsForReferencedEntities(Collections.singletonList(value), ctx);
			final URI uri = getIdentifier(value);
			assert uri != null;
			final Statement stmt = valueFactory.createStatement(models.primaryKey, property, uri);
			addStatement(stmt, ctx);
		}
	}
}
