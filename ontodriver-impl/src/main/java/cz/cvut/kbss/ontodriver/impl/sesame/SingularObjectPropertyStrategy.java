package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collections;
import java.util.logging.Level;

import org.openrdf.model.Statement;
import org.openrdf.model.URI;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
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

	private <T> void loadObjectProperty(Attribute<?, ?> property) throws OntoDriverException,
			IllegalArgumentException, IllegalAccessException {
		final URI propertyUri = getAddressAsSesameUri(property.getIRI());
		final Descriptor propertyDescriptor = models.descriptor.getAttributeDescriptor(property);
		final URI context = SesameUtils.toSesameUri(propertyDescriptor.getContext(), valueFactory);
		URI objectUri = getObjectPropertyValue(models.primaryKey, propertyUri,
				property.isInferred(), context);
		if (objectUri == null) {
			if (LOG.isLoggable(Level.FINER)) {
				LOG.finer("Value of object property " + property.getIRI()
						+ " not found or is not a resource.");
			}
			return;
		}
		final Object value = getJavaInstanceForSubject(property.getJavaType(), objectUri,
				propertyDescriptor);
		if (value != null) {
			property.getJavaField().set(models.entity, value);
		}
	}

	private void saveObjectProperty(URI property, Object value, Attribute<?, ?> att,
			boolean removeOld) throws OntoDriverException {
		final Descriptor descriptor = models.descriptor.getAttributeDescriptor(att);
		if (removeOld) {
			removeOldObjectPropertyValues(models.primaryKey, property,
					SesameUtils.toSesameUri(descriptor.getContext(), valueFactory));
		}
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("setObjectProperty '" + property + "' of " + models.primaryKey + " to "
					+ value);
		}
		if (value != null) {
			final URI context = SesameUtils.toSesameUri(descriptor.getContext(), valueFactory);
			addIndividualsForReferencedEntities(Collections.singletonList(value), context);
			final URI uri = getIdentifier(value);
			assert uri != null;
			final Statement stmt = valueFactory.createStatement(models.primaryKey, property, uri);
			addStatement(stmt, context);
		}
	}
}
