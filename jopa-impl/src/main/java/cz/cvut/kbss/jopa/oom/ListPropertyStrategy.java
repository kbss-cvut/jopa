/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

abstract class ListPropertyStrategy<L extends ListDescriptor, V extends ListValueDescriptor, X>
        extends PluralObjectPropertyStrategy<ListAttributeImpl<? super X, ?>, X> {

    ListPropertyStrategy(EntityType<X> et, ListAttributeImpl<? super X, ?> att, Descriptor descriptor,
                         EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    protected void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof List || value == null;
        extractListValues((List<?>) value, instance, valueBuilder);
    }

    /**
     * Adds elements of the list to the value descriptor.
     */
    <K> void addListElementsToListValueDescriptor(ListValueDescriptor listDescriptor, List<K> list) {
        if (list == null) {
            return;
        }
        final Class<?> elemType = attribute.getBindableJavaType();
        if (IdentifierTransformer.isValidIdentifierType(elemType)) {
            list.stream().filter(Objects::nonNull)
                .forEach(item -> listDescriptor.addValue(NamedResource.create(IdentifierTransformer.valueAsUri(item))));
        } else if (elemType.isEnum()) {
            assert attribute.getConverter() != null;
            list.stream().filter(Objects::nonNull).forEach(item -> listDescriptor.addValue(
                    (NamedResource) attribute.getConverter().convertToAxiomValue(item)));
        } else {
            final EntityType<?> valueType = mapper.getEntityType(elemType);
            addItemsToDescriptor(listDescriptor, list, valueType);
        }
    }

    static void addItemsToDescriptor(ListValueDescriptor listDescriptor, List<?> list, EntityType<?> valueType) {
        list.stream().filter(Objects::nonNull).forEach(item -> listDescriptor
                .addValue(NamedResource.create(EntityPropertiesUtils.getIdentifier(item, valueType))));
    }

    <K> List<K> resolveUnpersistedItems(List<K> list) {
        if (list == null || IdentifierTransformer.isValidIdentifierType(
                attribute.getBindableJavaType()) || attribute.getBindableJavaType().isEnum()) {
            return Collections.emptyList();
        } else {
            return list.stream().filter(item -> item != null && !referenceSavingResolver
                    .shouldSaveReferenceToItem(item, getAttributeValueContexts())).collect(Collectors.toList());
        }
    }

    abstract L createListDescriptor(Axiom<?> ax);

    abstract V createListValueDescriptor(X instance);

    abstract <K> void extractListValues(List<K> list, X instance, AxiomValueGatherer valueBuilder);
}
