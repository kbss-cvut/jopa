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

	public SingularAnnotationStrategy(SesameModuleInternal internal) {
		super(internal);
	}

	@Override
	<T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad)
			throws IllegalArgumentException, IllegalAccessException {
		loadAnnotationProperty(entity, uri, att);
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
	private <T> void loadAnnotationProperty(T instance, URI uri, Attribute<?, ?> property)
			throws IllegalArgumentException, IllegalAccessException {
		final URI annotationProperty = getAddressAsSesameUri(property.getIRI());
		Model res = storage.filter(uri, annotationProperty, null, false);
		if (res.isEmpty()) {
			res = storage.filter(uri, annotationProperty, null, true);
		}
		Object value = null;
		URI datatype = null;
		for (Statement stmt : res) {
			final Value val = stmt.getObject();
			if (!(val instanceof Literal)) {
				continue;
			}
			final Literal lit = (Literal) val;
			if (!lit.getLanguage().equals(lang)) {
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
			property.getJavaField().set(instance, value);
		}
	}
}
