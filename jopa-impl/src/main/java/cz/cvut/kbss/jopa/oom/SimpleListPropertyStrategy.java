/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

import java.net.URI;
import java.util.Collection;
import java.util.List;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

class SimpleListPropertyStrategy<X> extends
        ListPropertyStrategy<SimpleListDescriptor, SimpleListValueDescriptor, X> {

    public SimpleListPropertyStrategy(EntityType<X> et, ListAttribute<? super X, ?> att,
                                      Descriptor descriptor, EntityMappingHelper mapper) {
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
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(listAttribute
                .getIRI().toURI(), listAttribute.isInferred());
        final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(listAttribute
                .getOWLObjectPropertyHasNextIRI().toURI(), listAttribute.isInferred());
        final SimpleListDescriptor listDescriptor = new SimpleListDescriptorImpl(owner,
                listProperty, nextNodeProperty);
        listDescriptor.setContext(getAttributeContext());
        return listDescriptor;
    }

    @Override
    <K> void extractListValues(List<K> list, X instance, AxiomValueGatherer valueBuilder) {
        final SimpleListValueDescriptor listDescriptor = createListValueDescriptor(instance);
        final Class<K> elemType = (Class<K>) listAttribute.getBindableJavaType();
        final EntityType<K> valueType = mapper.getEntityType(elemType);
        if (list != null) {
            for (K item : list) {
                final URI id = resolveValueIdentifier(item, valueType);
                cascadeResolver.resolveFieldCascading(listAttribute, item, getAttributeContext());
                listDescriptor.addValue(NamedResource.create(id));
            }
        }
        valueBuilder.addSimpleListValues(listDescriptor);
    }

    @Override
    SimpleListValueDescriptor createListValueDescriptor(X instance) {
        final NamedResource owner = NamedResource.create(resolveValueIdentifier(instance, et));
        final Assertion listProperty = Assertion
                .createObjectPropertyAssertion(listAttribute.getIRI().toURI(), listAttribute.isInferred());
        final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(listAttribute
                .getOWLObjectPropertyHasNextIRI().toURI(), listAttribute.isInferred());
        final SimpleListValueDescriptor listDescriptor = new SimpleListValueDescriptor(owner,
                listProperty, nextNodeProperty);
        listDescriptor.setContext(getAttributeContext());
        return listDescriptor;
    }
}
