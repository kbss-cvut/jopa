package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collection;
import java.util.List;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class ReferencedListPropertyStrategy<X> extends
		ListPropertyStrategy<ReferencedListDescriptor, ReferencedListValueDescriptor, X> {

	public ReferencedListPropertyStrategy(EntityType<X> et, ListAttribute<? super X, ?> att,
			Descriptor descriptor, EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final ReferencedListDescriptor listDescriptor = createListDescriptor(ax);
		final Collection<Axiom<NamedResource>> sequence = mapper.loadReferencedList(listDescriptor);
		for (Axiom<?> a : sequence) {
			if (a.getAssertion().getIdentifier()
					.equals(listAttribute.getOWLPropertyHasContentsIRI().toURI())) {
				super.addValueFromAxiom(a);
			}
		}
	}

	@Override
	ReferencedListDescriptor createListDescriptor(Axiom<?> ax) {
		final NamedResource owner = ax.getSubject();

		final boolean inferred = listAttribute.isInferred();
		final Assertion listProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getIRI().toURI(), inferred);
		final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLObjectPropertyHasNextIRI().toURI(), inferred);
		final Assertion nodeContentProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLPropertyHasContentsIRI().toURI(), inferred);
		final ReferencedListDescriptor listDescriptor = new ReferencedListDescriptorImpl(owner,
				listProperty, nextNodeProperty, nodeContentProperty);
		listDescriptor.setContext(getAttributeContext());
		return listDescriptor;
	}

	@Override
	<K> void extractListValues(List<K> list, X instance, AxiomValueGatherer valueBuilder) {
		final ReferencedListValueDescriptor listDescriptor = createListValueDescriptor(instance);
		final Class<K> elemType = (Class<K>) listAttribute.getBindableJavaType();
		final EntityType<K> valueType = mapper.getEntityType(elemType);
		if (list != null) {
			for (K item : list) {
				final URI itemUri = resolveValueIdentifier(item, valueType);
				cascadeResolver.resolveFieldCascading(pluralAtt, item, getAttributeContext());
				listDescriptor.addValue(NamedResource.create(itemUri));
			}
		}
		valueBuilder.addReferencedListValues(listDescriptor);
	}

	@Override
	ReferencedListValueDescriptor createListValueDescriptor(X instance) {
		final URI owner = EntityPropertiesUtils.getPrimaryKey(instance, et);
		final Assertion hasList = Assertion.createObjectPropertyAssertion(listAttribute.getIRI()
				.toURI(), listAttribute.isInferred());
		final Assertion hasNext = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLObjectPropertyHasNextIRI().toURI(), listAttribute.isInferred());
		final Assertion hasContent = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLPropertyHasContentsIRI().toURI(), listAttribute.isInferred());
		final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(
				NamedResource.create(owner), hasList, hasNext, hasContent);
		descriptor.setContext(getAttributeContext());
		return descriptor;
	}
}
