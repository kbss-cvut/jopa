package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SimpleSetValueExtractor extends PluralAttributeValueExtractor {

	SimpleSetValueExtractor(PluralObjectPropertyStrategy parent, PluralAttribute<?, ?, ?> attribute) {
		super(parent, attribute);
	}

	@Override
	<T> Map<Assertion, Collection<Value<?>>> extractValues(Collection<T> values,
			Descriptor descriptor) {
		final Map<Assertion, Collection<Value<?>>> map = new HashMap<>(4);
		final Set<Value<?>> assertionValues = new HashSet<>(values.size());
		map.put(parent.createAssertion(), assertionValues);
		for (T val : values) {
			final EntityType<T> et = (EntityType<T>) parent.mapper.getEntityType(val.getClass());
			final URI id = parent.resolveValueIdentifier(val, et);
			parent.cascadeResolver.resolveFieldCascading(attribute, val, descriptor
					.getAttributeDescriptor(attribute).getContext());
			assertionValues.add(new Value<>(id));
		}
		return map;
	}
}
