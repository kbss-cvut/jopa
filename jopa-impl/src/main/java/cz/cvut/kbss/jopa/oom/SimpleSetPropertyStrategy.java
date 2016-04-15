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
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

class SimpleSetPropertyStrategy<X> extends PluralObjectPropertyStrategy<X> {

    SimpleSetPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att, Descriptor descriptor,
                                     EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        extractValues(valueCollection, valueBuilder);
    }

    private <T> void extractValues(Collection<T> valueCollection, AxiomValueGatherer valueBuilder) {
        if (valueCollection == null) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
            return;
        }
        final Set<Value<?>> assertionValues = new HashSet<>(valueCollection.size());
        if (IdentifierTransformer.isValidIdentifierType(pluralAtt.getBindableJavaType())) {
            valueCollection.stream().filter(item -> item != null).forEach(item -> assertionValues
                    .add(new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(item)))));
        } else {
            final EntityType<T> et = (EntityType<T>) mapper.getEntityType(pluralAtt.getBindableJavaType());
            for (T val : valueCollection) {
                if (val == null) {
                    continue;
                }
                final URI id = resolveValueIdentifier(val, et);
                cascadeResolver.resolveFieldCascading(attribute, val, getAttributeContext());
                assertionValues.add(new Value<>(NamedResource.create(id)));
            }
        }
        valueBuilder.addValues(createAssertion(), assertionValues, getAttributeContext());
    }
}
