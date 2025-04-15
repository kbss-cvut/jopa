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

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Collections;
import java.util.Set;

class SingularDataPropertyStrategy<X> extends DataPropertyFieldStrategy<AbstractAttribute<? super X, ?>, X> {

    Object value;

    SingularDataPropertyStrategy(EntityType<X> et, AbstractAttribute<? super X, ?> att,
                                 Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        final Object val = ax.getValue().getValue();
        if (!isValidRange(val)) {
            return;
        }
        verifyCardinalityConstraint(ax.getSubject());
        this.value = toAttributeValue(val);
    }

    void verifyCardinalityConstraint(NamedResource subject) {
        if (value != null) {
            throw new CardinalityConstraintViolatedException(
                    "Expected single value of attribute " + attribute.getName() + " of instance " + subject +
                            ", but got multiple.");
        }
    }

    @Override
    boolean hasValue() {
        return value != null;
    }

    @Override
    void buildInstanceFieldValue(Object entity) {
        setValueOnInstance(entity, value);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        valueBuilder.addValue(createAssertion(), extractValue(instance), getAttributeWriteContext());
    }

    private Value<?> extractValue(X instance) {
        final Object extractedValue = extractFieldValueFromInstance(instance);
        return extractedValue != null ? convertToAxiomValue(extractedValue) : Value.nullValue();
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        final Value<?> val = extractValue(instance);
        if (Value.nullValue().equals(val)) {
            return Collections.emptySet();
        }
        return Collections.singleton(
                new AxiomImpl<>(NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et)),
                                createAssertion(), val));
    }
}
