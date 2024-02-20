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
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.Collection;
import java.util.List;

class SimpleListPropertyStrategy<X> extends
        ListPropertyStrategy<SimpleListDescriptor, SimpleListValueDescriptor, X> {

    SimpleListPropertyStrategy(EntityType<X> et, ListAttributeImpl<? super X, ?> att, Descriptor descriptor,
                               EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final SimpleListDescriptor listDescriptor = createListDescriptor(ax);
        final Collection<Axiom<NamedResource>> sequence = mapper.loadSimpleList(listDescriptor);
        sequence.forEach(super::addValueFromAxiom);
    }

    SimpleListDescriptor createListDescriptor(Axiom<?> ax) {
        final NamedResource owner = ax.getSubject();
        final SimpleListDescriptor listDescriptor = ListDescriptorFactory.createSimpleListDescriptor(owner, attribute);
        listDescriptor.setContext(getAttributeWriteContext());
        return listDescriptor;
    }

    @Override
    <K> void extractListValues(List<K> list, X instance, AxiomValueGatherer valueBuilder) {
        final SimpleListValueDescriptor listDescriptor = createListValueDescriptor(instance);
        final List<K> pendingItems = resolveUnpersistedItems(list);
        if (!pendingItems.isEmpty()) {
            pendingItems.forEach(item -> referenceSavingResolver.registerPendingReference(item, listDescriptor, list));
            return;
        }
        addListElementsToListValueDescriptor(listDescriptor, list);
        valueBuilder.addSimpleListValues(listDescriptor);
    }

    SimpleListValueDescriptor createListValueDescriptor(X instance) {
        final NamedResource owner = NamedResource.create(resolveValueIdentifier(instance, et));
        final SimpleListValueDescriptor listDescriptor = ListDescriptorFactory.createSimpleListValueDescriptor(owner, attribute);
        listDescriptor.setContext(getAttributeWriteContext());
        return listDescriptor;
    }
}
