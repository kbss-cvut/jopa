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
		return createImpl(att, internal, null);
	}

	/**
	 * Creates strategy for loading and saving of the specified attribute.
	 * 
	 * @param att
	 *            Attribute
	 * @param internal
	 *            Instance which does the object-ontological mapping
	 * @param models
	 *            Models with statements about a subject
	 * @return Strategy instance
	 */
	static AttributeStrategy createStrategy(Attribute<?, ?> att, SesameModuleInternal internal,
			SubjectModels models) {
		return createImpl(att, internal, models);
	}

	private static AttributeStrategy createImpl(Attribute<?, ?> att, SesameModuleInternal internal,
			SubjectModels models) {
		AttributeStrategy s = null;
		if (att.isCollection()) {
			switch (att.getPersistentAttributeType()) {
			case ANNOTATION:
				s = new PluralAnnotationStrategy(internal, models);
				break;
			case DATA:
				s = new PluralDataPropertyStrategy(internal, models);
				break;
			case OBJECT:
				s = new PluralObjectPropertyStrategy(internal, models);
				break;
			default:
				throw new IllegalArgumentException("Unsupported attribute type "
						+ att.getPersistentAttributeType());
			}
		} else {
			switch (att.getPersistentAttributeType()) {
			case ANNOTATION:
				s = new SingularAnnotationStrategy(internal, models);
				break;
			case DATA:
				s = new SingularDataPropertyStrategy(internal, models);
				break;
			case OBJECT:
				s = new SingularObjectPropertyStrategy(internal, models);
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
