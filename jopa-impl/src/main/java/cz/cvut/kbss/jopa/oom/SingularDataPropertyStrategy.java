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

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Value;

class SingularDataPropertyStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

    private Object value;

    SingularDataPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att,
                                 Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final Value<?> val = ax.getValue();
        if (!isValidRange(val.getValue())) {
            return;
        }
        if (value != null) {
            throw new CardinalityConstraintViolatedException(
                    "Expected single value of attribute " + attribute.getName() + ", but got multiple");
        }
        this.value = val.getValue();
    }

    private boolean isValidRange(Object value) {
        return attribute.getJavaField().getType().isAssignableFrom(value.getClass()) || isFieldEnum();
    }

    private boolean isFieldEnum() {
        final Class<?> cls = attribute.getJavaField().getType();
        return cls.isEnum();
    }

    @Override
    void buildInstanceFieldValue(Object entity) throws IllegalAccessException {
        final Object toAssign = isFieldEnum() ? resolveEnumValue() : value;
        setValueOnInstance(entity, toAssign);
    }

    private Object resolveEnumValue() {
        final Class cls = attribute.getJavaField().getType();
        return Enum.valueOf(cls, value.toString());
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        final Object extractedValue = extractFieldValueFromInstance(instance);

        final Value<?> val = extractedValue != null ? new Value<>(extractedValue) : Value.nullValue();
        valueBuilder.addValue(createAssertion(), val, getAttributeContext());
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(),
                attribute.isInferred());
    }
}
