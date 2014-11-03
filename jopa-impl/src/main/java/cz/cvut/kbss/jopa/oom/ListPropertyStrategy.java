package cz.cvut.kbss.jopa.oom;

import java.util.List;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.ontodriver_new.descriptors.ListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

abstract class ListPropertyStrategy<L extends ListDescriptor, V extends ListValueDescriptor, X>
		extends PluralObjectPropertyStrategy<X> {

	protected final ListAttribute<? super X, ?> listAttribute;

	public ListPropertyStrategy(EntityType<X> et,
			ListAttribute<? super X, ?> att, Descriptor descriptor,
			EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
		this.listAttribute = att;
	}

	@Override
	protected void buildAxiomValuesFromInstance(X instance,
			AxiomValueGatherer valueBuilder) throws IllegalArgumentException,
			IllegalAccessException {
		final Object value = extractFieldValueFromInstance(instance);
		assert (value instanceof List || value == null);
		extractListValues((List<?>) value, instance, valueBuilder);
	}

	abstract L createListDescriptor(Axiom<?> ax);

	abstract V createListValueDescriptor(X instance);

	abstract <K> void extractListValues(List<K> list, X instance,
			AxiomValueGatherer valueBuilder);
}
