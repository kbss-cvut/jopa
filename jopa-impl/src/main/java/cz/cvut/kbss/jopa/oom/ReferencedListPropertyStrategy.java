/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.Collection;
import java.util.List;

class ReferencedListPropertyStrategy<X> extends
        ListPropertyStrategy<ReferencedListDescriptor, ReferencedListValueDescriptor<NamedResource>, X> {

    ReferencedListPropertyStrategy(EntityType<X> et, ListAttributeImpl<? super X, ?> att, Descriptor descriptor,
                                   EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        final ReferencedListDescriptor listDescriptor = createListDescriptor(ax);
        final Collection<Axiom<?>> sequence = mapper.loadReferencedList(listDescriptor);
        sequence.stream()
                .filter(a -> a.getAssertion().getIdentifier().equals(attribute.getHasContentsPropertyIRI().toURI()))
                .forEach(super::addAxiomValue);
    }

    ReferencedListDescriptor createListDescriptor(Axiom<?> ax) {
        final NamedResource owner = ax.getSubject();
        final ReferencedListDescriptor listDescriptor = ListDescriptorFactory.createReferencedListDescriptor(owner, attribute);
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
        final NamedResource owner = NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et));
        final ReferencedListValueDescriptor<V> descriptor = ListDescriptorFactory.createReferencedListValueDescriptor(owner, attribute);
        descriptor.setContext(getAttributeWriteContext());
        return descriptor;
    }
}
