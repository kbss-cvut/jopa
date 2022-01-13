/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

class SingularDataPropertyStrategy<X> extends DataPropertyFieldStrategy<AbstractAttribute<? super X, ?>, X> {

    Object value;

    SingularDataPropertyStrategy(EntityType<X> et, AbstractAttribute<? super X, ?> att,
                                 Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
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
    void buildInstanceFieldValue(Object entity) {
        setValueOnInstance(entity, value);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object extractedValue = toAxiomValue(extractFieldValueFromInstance(instance));

        final Value<?> val = extractedValue != null ? new Value<>(extractedValue) : Value.nullValue();
        valueBuilder.addValue(createAssertion(), val, getAttributeWriteContext());
    }
}
