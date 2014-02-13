package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.logging.Level;

import org.openrdf.model.Literal;
import org.openrdf.model.Model;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

/**
 * Strategy for singular data property values. </p>
 * 
 * @author ledvima1
 * 
 */
public class SingularDataPropertyStrategy extends AttributeStrategy {

	public SingularDataPropertyStrategy(SesameModuleInternal internal) {
		super(internal);
	}

	protected SingularDataPropertyStrategy(SesameModuleInternal internal, SubjectModels models) {
		super(internal, models);
	}

	@Override
	<T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad)
			throws IllegalAccessException, IllegalArgumentException {
		loadDataProperty(entity, uri, att);
	}

	@Override
	<T> void save(T entity, URI uri, Attribute<?, ?> att, URI attUri, Object value) {
		saveDataProperty(uri, attUri, value);
	}

	/**
	 * Loads data property value for the specified entity instance.
	 * 
	 * @param instance
	 *            Entity instance
	 * @param uri
	 *            Entity primary key (i. e. subject URI)
	 * @param property
	 *            Attribute representing the data property
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 */
	private <T> void loadDataProperty(T instance, URI uri, Attribute<?, ?> property)
			throws IllegalArgumentException, IllegalAccessException {
		final URI propertyUri = getAddressAsSesameUri(property.getIRI());
		final Model res = filter(uri, propertyUri, null, property.isInferred());
		Object value = null;
		URI datatype = null;
		for (Statement stmt : res) {
			Value val = stmt.getObject();
			if (!(val instanceof Literal)) {
				continue;
			}
			Literal lit = (Literal) val;
			datatype = lit.getDatatype();
			value = SesameUtils.getDataPropertyValue(lit);
			break;
		}
		if (value == null && LOG.isLoggable(Level.FINER)) {
			LOG.finer("Value of data property " + property.getIRI()
					+ " not found or is not a literal.");
		}
		final Class<?> cls = property.getJavaType();
		if (value != null && !cls.isAssignableFrom(value.getClass())) {
			throw new IllegalStateException("The field type " + cls
					+ " cannot be established from the declared data type " + datatype
					+ ". The declared class is " + value.getClass());
		}
		if (value != null) {
			property.getJavaField().set(instance, value);
		}
	}

	/**
	 * Saves data property value for the specified subject. </p>
	 * 
	 * This method also removes any previous values of the property.
	 * 
	 * @param subject
	 *            Subject URI
	 * @param property
	 *            Property URI
	 * @param value
	 *            Property value
	 */
	private void saveDataProperty(URI subject, URI property, Object value) {
		removeOldDataPropertyValues(subject, property);
		Literal lit = SesameUtils.createDataPropertyLiteral(value, lang, valueFactory);
		final Statement stmt = valueFactory.createStatement(subject, property, lit);
		addStatement(stmt);
	}
}
