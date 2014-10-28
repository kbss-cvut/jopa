package cz.cvut.kbss.jopa.oom;

import java.util.Collection;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class ReferencedListPropertyStrategy<X> extends SimpleListPropertyStrategy<X> {

	public ReferencedListPropertyStrategy(EntityType<X> et, ListAttribute<? super X, ?> att,
			Descriptor descriptor, EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final ReferencedListDescriptor listDescriptor = createReferencedListDescriptor(ax);
		final Collection<Axiom<?>> sequence = mapper.loadReferencedList(listDescriptor);
		for (Axiom<?> a : sequence) {
			super.addValueFromAxiom(a);
		}
	}

	private ReferencedListDescriptor createReferencedListDescriptor(Axiom<?> ax) {
		final NamedResource owner = ax.getSubject();

		final boolean inferred = listAttribute.isInferred();
		final Assertion listProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLListClass().toURI(), inferred);
		final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLObjectPropertyHasNextIRI().toURI(), inferred);
		final Assertion nodeContentProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLPropertyHasContentsIRI().toURI(), inferred);
		final ReferencedListDescriptor listDescriptor = new ReferencedListDescriptorImpl(owner,
				listProperty, nextNodeProperty, nodeContentProperty);
		return listDescriptor;
	}
}
