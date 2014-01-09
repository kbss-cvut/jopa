package cz.cvut.kbss.ontodriver.impl.sesame;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

/**
 * Factory for creating strategy appropriate for the specified attribute type.
 * 
 * @author ledvima1
 * 
 */
abstract class AttributeStrategyFactory {

	private AttributeStrategyFactory() {
		// private constructor to prevent instantiation
	}

	/**
	 * Creates strategy for loading and saving of the specified attribute.
	 * 
	 * @param att
	 *            Attribute
	 * @param internal
	 *            Instance which does the object-ontological mapping
	 * @return Strategy instance
	 */
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
			default:
				throw new IllegalArgumentException("Unsupported attribute type "
						+ att.getPersistentAttributeType());
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
			default:
				throw new IllegalArgumentException("Unsupported attribute type "
						+ att.getPersistentAttributeType());
			}
		}
		assert s != null;
		return s;
	}
}
