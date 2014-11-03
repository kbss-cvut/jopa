package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collection;
import java.util.List;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class SimpleListPropertyStrategy<X>
		extends
		ListPropertyStrategy<SimpleListDescriptor, SimpleListValueDescriptor, X> {

	public SimpleListPropertyStrategy(EntityType<X> et,
			ListAttribute<? super X, ?> att, Descriptor descriptor,
			EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final SimpleListDescriptor listDescriptor = createListDescriptor(ax);
		final Collection<Axiom<?>> sequence = mapper
				.loadSimpleList(listDescriptor);
		for (Axiom<?> a : sequence) {
			super.addValueFromAxiom(a);
		}
	}

	@Override
	SimpleListDescriptor createListDescriptor(Axiom<?> ax) {
		final NamedResource owner = ax.getSubject();
		final Assertion listProperty = Assertion.createObjectPropertyAssertion(
				listAttribute.getIRI().toURI(), listAttribute.isInferred());
		final Assertion nextNodeProperty = Assertion
				.createObjectPropertyAssertion(listAttribute
						.getOWLObjectPropertyHasNextIRI().toURI(),
						listAttribute.isInferred());
		final SimpleListDescriptor listDescriptor = new SimpleListDescriptorImpl(
				owner, listProperty, nextNodeProperty);
		return listDescriptor;
	}

	<K> void extractListValues(List<K> list, X instance,
			AxiomValueGatherer valueBuilder) {
		final SimpleListValueDescriptor listDescriptor = createListValueDescriptor(instance);
		final Class<K> elemType = (Class<K>) listAttribute
				.getBindableJavaType();
		final EntityType<K> valueType = mapper.getEntityType(elemType);
		if (list != null) {
			for (K item : list) {
				final URI id = resolveValueIdentifier(item, valueType);
				cascadeResolver.resolveFieldCascading(listAttribute, item,
						getAttributeContext());
				listDescriptor.addValue(NamedResource.create(id));
			}
		}
		valueBuilder.addSimpleListValues(listDescriptor);
	}

	@Override
	SimpleListValueDescriptor createListValueDescriptor(X instance) {
		final NamedResource owner = NamedResource
				.create(resolveValueIdentifier(instance, et));
		final Assertion listProperty = Assertion.createObjectPropertyAssertion(
				listAttribute.getIRI().toURI(), listAttribute.isInferred());
		final Assertion nextNodeProperty = Assertion
				.createObjectPropertyAssertion(listAttribute
						.getOWLObjectPropertyHasNextIRI().toURI(),
						listAttribute.isInferred());
		final SimpleListValueDescriptor listDescriptor = new SimpleListValueDescriptor(
				owner, listProperty, nextNodeProperty);
		listDescriptor.setContext(descriptor.getAttributeDescriptor(attribute)
				.getContext());
		return listDescriptor;
	}
}
