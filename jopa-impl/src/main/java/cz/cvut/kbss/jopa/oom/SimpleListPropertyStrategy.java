/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
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

    @Override
    SimpleListDescriptor createListDescriptor(Axiom<?> ax) {
        final NamedResource owner = ax.getSubject();
        final Assertion listProperty = Assertion
                .createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
        final Assertion nextNodeProperty = Assertion
                .createObjectPropertyAssertion(attribute.getOWLObjectPropertyHasNextIRI().toURI(),
                        attribute.isInferred());
        final SimpleListDescriptor listDescriptor = new SimpleListDescriptorImpl(owner, listProperty, nextNodeProperty);
        listDescriptor.setContext(getAttributeContext());
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

    @Override
    SimpleListValueDescriptor createListValueDescriptor(X instance) {
        final NamedResource owner = NamedResource.create(resolveValueIdentifier(instance, et));
        final Assertion listProperty = Assertion
                .createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
        final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(attribute
                .getOWLObjectPropertyHasNextIRI().toURI(), attribute.isInferred());
        final SimpleListValueDescriptor listDescriptor = new SimpleListValueDescriptor(owner, listProperty,
                nextNodeProperty);
        listDescriptor.setContext(getAttributeContext());
        return listDescriptor;
    }
}
