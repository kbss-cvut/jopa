package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.model.URI;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;

/**
 * Strategy for plural annotation property values. </p>
 * 
 * @author ledvima1
 * 
 */
class PluralAnnotationStrategy extends PluralDataPropertyStrategy {

	protected PluralAnnotationStrategy(SesameModuleInternal internal) {
		super(internal);
	}

	protected PluralAnnotationStrategy(SesameModuleInternal internal, SubjectModels models) {
		super(internal, models);
	}

	@Override
	<T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad) {
		throw new NotYetImplementedException(
				"Collection annotation properties are not implemented yet.");
	}

	@Override
	<T> void save(T entity, URI uri, Attribute<?, ?> att, URI attUri, Object value) {
		super.save(entity, uri, att, attUri, value);
	}
}
