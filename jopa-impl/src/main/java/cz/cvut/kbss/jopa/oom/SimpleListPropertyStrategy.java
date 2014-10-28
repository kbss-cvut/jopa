package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SimpleListPropertyStrategy<X> extends PluralObjectPropertyStrategy<X> {

	protected final ListAttribute<? super X, ?> listAttribute;

	public SimpleListPropertyStrategy(EntityType<X> et, ListAttribute<? super X, ?> att,
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
		final SimpleListDescriptor listDescriptor = new SimpleListDescriptorImpl(owner,
				listProperty, nextNodeProperty);
		return listDescriptor;
	}

	@Override
	Map<Assertion, Collection<Value<?>>> extractAttributeValuesFromInstance(X instance)
			throws IllegalArgumentException, IllegalAccessException {
		final Object value = extractFieldValueFromInstance(instance);
		if (value == null) {
			return Collections.emptyMap();
		}
		assert (value instanceof List);
		persistSimpleListValues((List<?>) value, instance);
		return Collections.emptyMap();
	}
	
	private <K> void persistSimpleListValues(List<K> list, X instance) {
		final SimpleListValueDescriptor listDescriptor = createListValueDescriptor(instance);
		final Class<K> elemType = (Class<K>) listAttribute.getElementType().getJavaType();
		final EntityType<K> valueType = mapper.getEntityType(elemType);
		for (K item : list) {
			final URI id = resolveValueIdentifier(item, valueType);
			listDescriptor.addValue(NamedResource.create(id));
		}
		mapper.persistSimpleList(listDescriptor);
	}

	private SimpleListValueDescriptor createListValueDescriptor(X instance) {
		final NamedResource owner = NamedResource.create(resolveValueIdentifier(instance, et));
		final Assertion listProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getIRI().toURI(), listAttribute.isInferred());
		final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(listAttribute
				.getOWLObjectPropertyHasNextIRI().toURI(), listAttribute.isInferred());
		final SimpleListValueDescriptor listDescriptor = new SimpleListValueDescriptor(owner,
				listProperty, nextNodeProperty);
		listDescriptor.setContext(descriptor.getAttributeDescriptor(attribute).getContext());
		return listDescriptor;
	}
}
