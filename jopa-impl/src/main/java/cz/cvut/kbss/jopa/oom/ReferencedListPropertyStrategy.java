/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.List;

class ReferencedListPropertyStrategy<X> extends
        ListPropertyStrategy<ReferencedListDescriptor, ReferencedListValueDescriptor<NamedResource>, X> {

    ReferencedListPropertyStrategy(EntityType<X> et, ListAttributeImpl<? super X, ?> att, Descriptor descriptor,
                                   EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final ReferencedListDescriptor listDescriptor = createListDescriptor(ax);
        final Collection<Axiom<?>> sequence = mapper.loadReferencedList(listDescriptor);
        sequence.stream()
                .filter(a -> a.getAssertion().getIdentifier().equals(attribute.getOWLPropertyHasContentsIRI().toURI()))
                .forEach(super::addValueFromAxiom);
    }

    ReferencedListDescriptor createListDescriptor(Axiom<?> ax) {
        final NamedResource owner = ax.getSubject();

        final boolean inferred = attribute.isInferred();
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion nextNodeProperty = Assertion
                .createObjectPropertyAssertion(attribute.getOWLObjectPropertyHasNextIRI().toURI(), inferred);
        final Assertion nodeContentProperty = Assertion.createObjectPropertyAssertion(attribute.getOWLPropertyHasContentsIRI()
                                                                                               .toURI(), inferred);
        final ReferencedListDescriptor listDescriptor = new ReferencedListDescriptorImpl(owner, listProperty,
                nextNodeProperty, nodeContentProperty);
        listDescriptor.setContext(getAttributeWriteContext());
        return listDescriptor;
    }

    @Override
    <K> void extractListValues(List<K> list, X instance, AxiomValueGatherer valueBuilder) {
        final ReferencedListValueDescriptor<K> listDescriptor = createListValueDescriptor(instance);
        final List<K> pendingItems = resolveUnpersistedItems(list);
        if (!pendingItems.isEmpty()) {
            pendingItems.forEach(item -> referenceSavingResolver.registerPendingReference(item, listDescriptor, list));
            return;
        }
        addListElementsToListValueDescriptor(listDescriptor, list);
        valueBuilder.addReferencedListValues(listDescriptor);
    }

    <V> ReferencedListValueDescriptor<V> createListValueDescriptor(X instance) {
        final URI owner = EntityPropertiesUtils.getIdentifier(instance, et);
        final boolean inferred = attribute.isInferred();
        final Assertion hasList = Assertion
                .createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion hasNext = Assertion.createObjectPropertyAssertion(attribute
                .getOWLObjectPropertyHasNextIRI().toURI(), inferred);
        final Assertion hasContent = Assertion.createObjectPropertyAssertion(attribute.getOWLPropertyHasContentsIRI()
                                                                                      .toURI(), inferred);
        final ReferencedListValueDescriptor<V> descriptor = new ReferencedListValueDescriptor<>(
                NamedResource.create(owner), hasList, hasNext, hasContent);
        descriptor.setContext(getAttributeWriteContext());
        return descriptor;
    }
}
