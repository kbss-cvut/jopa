package cz.cvut.kbss.jopa.oom;

import java.util.Collection;
import java.util.Map;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Value;

abstract class PluralAttributeValueExtractor {

	final PluralObjectPropertyStrategy parent;
	final PluralAttribute<?, ?, ?> attribute;

	protected PluralAttributeValueExtractor(PluralObjectPropertyStrategy parent,
			PluralAttribute<?, ?, ?> attribute) {
		this.parent = parent;
		this.attribute = attribute;
	}

	abstract <T> Map<Assertion, Collection<Value<?>>> extractValues(Collection<T> value,
			Descriptor descriptor);

	static PluralAttributeValueExtractor create(PluralObjectPropertyStrategy parent,
			PluralAttribute<?, ?, ?> attribute) {
		switch (attribute.getCollectionType()) {
		case COLLECTION:
		case LIST:
			// TODO
			return null;
		case SET:
			return new SimpleSetValueExtractor(parent, attribute);
		case MAP:
		default:
			throw new UnsupportedOperationException("Unsupported collection type "
					+ attribute.getCollectionType());
		}
	}
}
