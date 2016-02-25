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

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;

class SingularObjectPropertyStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

    private Object value;

    SingularObjectPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att,
                                   Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        assert ax.getValue().getValue() instanceof NamedResource;
        final NamedResource valueIdentifier = (NamedResource) ax.getValue().getValue();
        final Object newValue = mapper
                .getEntityFromCacheOrOntology(attribute.getJavaType(), valueIdentifier.getIdentifier(),
                        attributeDescriptor);
        if (value != null) {
            throw new CardinalityConstraintViolatedException(
                    "Expected single value of attribute " + attribute.getName() + " but got multiple.");
        }
        this.value = newValue;
    }

    @Override
    void buildInstanceFieldValue(Object instance) throws IllegalAccessException {
        setValueOnInstance(instance, value);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        final Object extractedValue = extractFieldValueFromInstance(instance);
        Value<?> val = extractedValue != null ? extractReferenceIdentifier(extractedValue) : Value.nullValue();
        valueBuilder.addValue(createAssertion(), val, getAttributeContext());
    }

    private <V> Value<NamedResource> extractReferenceIdentifier(final V value) {
        final EntityType<V> valEt = (EntityType<V>) mapper.getEntityType(value.getClass());
        if (valEt == null) {
            throw new EntityDeconstructionException("Value of field " + attribute.getJavaField()
                    + " is not a recognized entity.");
        }
        final URI id = resolveValueIdentifier(value, valEt);
        cascadeResolver.resolveFieldCascading(attribute, value, getAttributeContext());
        return new Value<>(NamedResource.create(id));
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(),
                attribute.isInferred());
    }
}
