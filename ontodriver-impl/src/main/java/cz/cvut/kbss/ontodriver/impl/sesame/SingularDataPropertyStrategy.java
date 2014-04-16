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

	protected SingularDataPropertyStrategy(SesameModuleInternal internal, SubjectModels<?> models) {
		super(internal, models);
	}

	@Override
	<T> void load(Attribute<?, ?> att, boolean alwaysLoad) throws IllegalAccessException,
			IllegalArgumentException {
		loadDataProperty(att);
	}

	@Override
	<T> void save(Attribute<?, ?> att, Object value, boolean removeOld) {
		final URI attUri = getAddressAsSesameUri(att.getIRI());
		saveDataProperty(attUri, value, att, removeOld);
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
	private <T> void loadDataProperty(Attribute<?, ?> property) throws IllegalArgumentException,
			IllegalAccessException {
		final URI subjectUri = models.primaryKey;
		final URI propertyUri = getAddressAsSesameUri(property.getIRI());
		final URI ctx = models.getFieldContext(property.getName());
		final Model res = filter(subjectUri, propertyUri, null, property.isInferred(), ctx);
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
			property.getJavaField().set(models.entity, value);
		}
	}

	/**
	 * Saves data property value for the specified subject. </p>
	 * 
	 * @param property
	 *            Property URI
	 * @param value
	 *            Property value
	 * @param att
	 *            Attribute descriptor
	 * @param removeOld
	 *            Whether to remove any old values of the specified attribute
	 */
	private void saveDataProperty(URI property, Object value, Attribute<?, ?> att, boolean removeOld) {
		final URI ctx = models.getFieldContext(att.getName());
		if (removeOld) {
			removeOldDataPropertyValues(models.primaryKey, property, ctx);
		}
		if (value == null) {
			return;
		}
		Literal lit = SesameUtils.createDataPropertyLiteral(value, lang, valueFactory);
		final Statement stmt = valueFactory.createStatement(models.primaryKey, property, lit);
		addStatement(stmt, ctx);
	}
}
