package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.logging.Level;

import org.openrdf.model.Literal;
import org.openrdf.model.Model;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

/**
 * Strategy for singular annotation property values.
 * 
 * @author ledvima1
 * 
 */
class SingularAnnotationStrategy extends SingularDataPropertyStrategy {

	protected SingularAnnotationStrategy(SesameModuleInternal internal, SubjectModels<?> models) {
		super(internal, models);
	}

	@Override
	<T> void load(Attribute<?, ?> att, boolean alwaysLoad) throws IllegalArgumentException,
			IllegalAccessException {
		loadAnnotationProperty(att);
	}

	/**
	 * Loads annotation property value for the specified entity instance.
	 * 
	 * @param instance
	 *            Entity instance
	 * @param uri
	 *            Entity primary key (i. e. subject URI)
	 * @param property
	 *            Attribute representing the annotation property
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 */
	private <T> void loadAnnotationProperty(Attribute<?, ?> property)
			throws IllegalArgumentException, IllegalAccessException {
		final URI annotationProperty = getAddressAsSesameUri(property.getIRI());
		final URI ctx = models.getFieldContext(property.getName());
		final Model res = filter(models.primaryKey, annotationProperty, null,
				property.isInferred(), ctx);
		Object value = null;
		URI datatype = null;
		for (Statement stmt : res) {
			final Value val = stmt.getObject();
			if (!(val instanceof Literal)) {
				continue;
			}
			final Literal lit = (Literal) val;
			if (lang != null && !lang.equals(lit.getLanguage())) {
				continue;
			}
			datatype = lit.getDatatype();
			value = SesameUtils.getDataPropertyValue(lit);
		}
		if (value == null && LOG.isLoggable(Level.FINER)) {
			LOG.finer("Value of annotation property " + property.getIRI()
					+ " not found, is not a literal or is not in the expected language.");
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
}
