/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.List;

abstract class ListPropertyStrategy<L extends ListDescriptor, V extends ListValueDescriptor, X>
        extends PluralObjectPropertyStrategy<X> {

    final ListAttribute<? super X, ?> listAttribute;

    public ListPropertyStrategy(EntityType<X> et, ListAttribute<? super X, ?> att, Descriptor descriptor,
                                EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
        this.listAttribute = att;
    }

    @Override
    protected void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
            throws IllegalAccessException {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof List || value == null;
        extractListValues((List<?>) value, instance, valueBuilder);
    }

    <K> void addListElementsToListValueDescriptor(ListValueDescriptor listDescriptor, List<K> list) {
        if (list == null) {
            return;
        }
        if (IdentifierTransformer.isValidIdentifierType(pluralAtt.getBindableJavaType())) {
            list.stream().filter(item -> item != null)
                .forEach(item -> listDescriptor.addValue(NamedResource.create(IdentifierTransformer.valueAsUri(item))));
        } else {
            final Class<K> elemType = (Class<K>) listAttribute.getBindableJavaType();
            final EntityType<K> valueType = mapper.getEntityType(elemType);
            for (K item : list) {
                if (item == null) {
                    continue;
                }
                final URI itemUri = resolveValueIdentifier(item, valueType);
                cascadeResolver.resolveFieldCascading(pluralAtt, item, getAttributeContext());
                listDescriptor.addValue(NamedResource.create(itemUri));
            }
        }
    }

    abstract L createListDescriptor(Axiom<?> ax);

    abstract V createListValueDescriptor(X instance);

    abstract <K> void extractListValues(List<K> list, X instance, AxiomValueGatherer valueBuilder);
}
