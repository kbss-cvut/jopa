/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
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
        ListPropertyStrategy<ReferencedListDescriptor, ReferencedListValueDescriptor, X> {

    ReferencedListPropertyStrategy(EntityType<X> et, ListAttributeImpl<? super X, ?> att, Descriptor descriptor,
                                   EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final ReferencedListDescriptor listDescriptor = createListDescriptor(ax);
        final Collection<Axiom<NamedResource>> sequence = mapper.loadReferencedList(listDescriptor);
        sequence.stream()
                .filter(a -> a.getAssertion().getIdentifier().equals(attribute.getOWLPropertyHasContentsIRI().toURI()))
                .forEach(super::addValueFromAxiom);
    }

    @Override
    ReferencedListDescriptor createListDescriptor(Axiom<?> ax) {
        final NamedResource owner = ax.getSubject();

        final boolean inferred = attribute.isInferred();
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion nextNodeProperty = Assertion
                .createObjectPropertyAssertion(attribute.getOWLObjectPropertyHasNextIRI().toURI(), inferred);
        final Assertion nodeContentProperty = Assertion
                .createObjectPropertyAssertion(attribute.getOWLPropertyHasContentsIRI().toURI(), inferred);
        final ReferencedListDescriptor listDescriptor = new ReferencedListDescriptorImpl(owner, listProperty,
                nextNodeProperty, nodeContentProperty);
        listDescriptor.setContext(getAttributeContext());
        return listDescriptor;
    }

    @Override
    <K> void extractListValues(List<K> list, X instance, AxiomValueGatherer valueBuilder) {
        final ReferencedListValueDescriptor listDescriptor = createListValueDescriptor(instance);
        final List<K> pendingItems = resolveUnpersistedItems(list);
        if (!pendingItems.isEmpty()) {
            pendingItems.forEach(item -> referenceSavingResolver.registerPendingReference(item, listDescriptor, list));
            return;
        }
        addListElementsToListValueDescriptor(listDescriptor, list);
        valueBuilder.addReferencedListValues(listDescriptor);
    }

    @Override
    ReferencedListValueDescriptor createListValueDescriptor(X instance) {
        final URI owner = EntityPropertiesUtils.getIdentifier(instance, et);
        final Assertion hasList = Assertion
                .createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
        final Assertion hasNext = Assertion.createObjectPropertyAssertion(attribute
                .getOWLObjectPropertyHasNextIRI().toURI(), attribute.isInferred());
        final Assertion hasContent = Assertion.createObjectPropertyAssertion(attribute
                .getOWLPropertyHasContentsIRI().toURI(), attribute.isInferred());
        final ReferencedListValueDescriptor descriptor = new ReferencedListValueDescriptor(
                NamedResource.create(owner), hasList, hasNext, hasContent);
        descriptor.setContext(getAttributeContext());
        return descriptor;
    }
}
