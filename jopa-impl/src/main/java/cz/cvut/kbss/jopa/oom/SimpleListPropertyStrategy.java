package cz.cvut.kbss.jopa.oom;

import java.util.Collection;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.ontodriver_new.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class SimpleListPropertyStrategy extends PluralObjectPropertyStrategy {

	protected final ListAttribute<?, ?> listAttribute;

	public SimpleListPropertyStrategy(EntityType<?> et, ListAttribute<?, ?> att,
			Descriptor descriptor, EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
		this.listAttribute = att;
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final SimpleListDescriptor listDescriptor = createListDescriptor(ax);
		final Collection<Axiom<?>> sequence = mapper.loadSimpleList(listDescriptor);
		for (Axiom<?> a : sequence) {
			super.addValueFromAxiom(a);
		}
	}

	private SimpleListDescriptor createListDescriptor(Axiom<?> ax) {
		final NamedResource owner = ax.getSubject();
		final Assertion listProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getIRI().toURI(), listAttribute.isInferred());
		final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLObjectPropertyHasNextIRI().toURI(), listAttribute.isInferred());
		final SimpleListDescriptor listDescriptor = new SimpleListDescriptor(owner, listProperty,
				nextNodeProperty);
		return listDescriptor;
	}
}
