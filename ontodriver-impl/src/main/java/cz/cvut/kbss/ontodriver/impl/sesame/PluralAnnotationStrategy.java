package cz.cvut.kbss.ontodriver.impl.sesame;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;

/**
 * Strategy for plural annotation property values. </p>
 * 
 * @author ledvima1
 * 
 */
class PluralAnnotationStrategy extends PluralDataPropertyStrategy {

	protected PluralAnnotationStrategy(SesameModuleInternal internal, SubjectModels<?> models) {
		super(internal, models);
	}

	@Override
	<T> void load(Attribute<?, ?> att, boolean alwaysLoad) {
		throw new NotYetImplementedException(
				"Collection annotation properties are not implemented yet.");
	}
}
