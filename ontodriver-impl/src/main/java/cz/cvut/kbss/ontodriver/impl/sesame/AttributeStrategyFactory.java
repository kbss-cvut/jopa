package cz.cvut.kbss.ontodriver.impl.sesame;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

abstract class AttributeStrategyFactory {

	private AttributeStrategyFactory() {
		// private constructor to prevent instantiation
	}

	static AttributeStrategy createStrategy(Attribute<?, ?> att, SesameModuleInternal internal) {
		return createImpl(att, internal);
	}

	private static AttributeStrategy createImpl(Attribute<?, ?> att, SesameModuleInternal internal) {
		AttributeStrategy s = null;
		if (att.isCollection()) {
			switch (att.getPersistentAttributeType()) {
			case ANNOTATION:
				s = new PluralAnnotationStrategy(internal);
				break;
			case DATA:
				s = new PluralDataPropertyStrategy(internal);
				break;
			case OBJECT:
				s = new PluralObjectPropertyStrategy(internal);
				break;

			}
		} else {
			switch (att.getPersistentAttributeType()) {
			case ANNOTATION:
				s = new SingularAnnotationStrategy(internal);
				break;
			case DATA:
				s = new SingularDataPropertyStrategy(internal);
				break;
			case OBJECT:
				s = new SingularObjectPropertyStrategy(internal);
				break;
			}
		}
		assert s != null;
		return s;
	}
}
