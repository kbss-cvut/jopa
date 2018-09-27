/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

public class PluralDataPropertyStrategy<X> extends DataPropertyFieldStrategy<X> {

    private PluralAttribute<? super X, ?, ?> pluralAtt;

    private Collection<Object> values;

    PluralDataPropertyStrategy(EntityType<X> et, PluralAttribute<? super X, ?, ?> att, Descriptor attributeDescriptor,
                               EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
        this.pluralAtt = att;
        this.values = CollectionFactory.createDefaultCollection(pluralAtt.getCollectionType());
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final Object value = ax.getValue().getValue();
        if (isValidRange(value)) {
            this.values.add(valueResolver.fromAxiom(value));
        }
    }

    @Override
    boolean isValidRange(Object value) {
        return pluralAtt.getElementType().getJavaType().isAssignableFrom(value.getClass());
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        if (!values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
        } else {
            final Set<Value<?>> assertionValues = valueCollection.stream()
                                                                 .map(v -> new Value<>(valueResolver.toAxiomValue(v)))
                                                                 .collect(Collectors.toSet());
            valueBuilder.addValues(createAssertion(), assertionValues, getAttributeContext());
        }
    }
}
