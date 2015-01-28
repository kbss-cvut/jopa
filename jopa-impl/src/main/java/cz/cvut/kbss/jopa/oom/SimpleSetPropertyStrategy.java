package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SimpleSetPropertyStrategy<X> extends PluralObjectPropertyStrategy<X> {

	public SimpleSetPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att,
			Descriptor descriptor, EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
	}

	@Override
	void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
			throws IllegalArgumentException, IllegalAccessException {
		final Object value = extractFieldValueFromInstance(instance);
		assert value instanceof Collection || value == null;
		final Collection<?> valueCollection = (Collection<?>) value;
		extractValues(instance, valueCollection, valueBuilder);
	}

	private <T> void extractValues(X instance, Collection<T> valueCollection,
			AxiomValueGatherer valueBuilder) {
		if (valueCollection == null) {
			valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
			return;
		}
		final Set<Value<?>> assertionValues = new HashSet<>(valueCollection.size());
		for (T val : valueCollection) {
			if (val == null) {
				continue;
			}
			final EntityType<T> et = (EntityType<T>) mapper.getEntityType(val.getClass());
			final URI id = resolveValueIdentifier(val, et);
			cascadeResolver.resolveFieldCascading(attribute, val, getAttributeContext());
			assertionValues.add(new Value<>(NamedResource.create(id)));
		}
		valueBuilder.addValues(createAssertion(), assertionValues, getAttributeContext());
	}
}
