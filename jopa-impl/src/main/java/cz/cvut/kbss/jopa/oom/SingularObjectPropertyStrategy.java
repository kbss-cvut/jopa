/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

class SingularObjectPropertyStrategy<X> extends FieldStrategy<AbstractAttribute<? super X, ?>, X> {

    private static final Logger LOG = LoggerFactory.getLogger(SingularObjectPropertyStrategy.class);

    private Object value;

    SingularObjectPropertyStrategy(EntityType<X> et, AbstractAttribute<? super X, ?> att,
                                   Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        assert ax.getValue().getValue() instanceof NamedResource;
        final NamedResource valueIdentifier = (NamedResource) ax.getValue().getValue();
        final Class<?> targetType = attribute.getJavaType();
        final Object newValue;
        if (IdentifierTransformer.isValidIdentifierType(targetType)) {
            newValue = IdentifierTransformer.transformToIdentifier(valueIdentifier.getIdentifier(), targetType);
        } else if (targetType.isEnum()) {
            assert attribute.getConverter() != null;
            assert attribute.getConverter().supportsAxiomValueType(NamedResource.class);
            // Maybe we should catch the exception and just skip the value if it is not convertible to be consistent
            // with the 'else' branch behavior?
            newValue = attribute.getConverter().convertToAttribute(valueIdentifier);
        } else {
            newValue = mapper.getEntityFromCacheOrOntology(targetType, valueIdentifier.getIdentifier(),
                                                           entityDescriptor.getAttributeDescriptor(attribute));
            if (newValue == null) {
                LOG.trace("Value of axiom {} could not be loaded as entity filling attribute {}.", ax, attribute);
                return;
            }
        }
        verifyCardinality(ax.getSubject());
        this.value = newValue;
    }

    private void verifyCardinality(NamedResource subject) {
        if (value != null) {
            throw new CardinalityConstraintViolatedException(
                    "Expected single value of attribute " + attribute.getName() + " of instance " + subject +
                            ", but got multiple.");
        }
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        setValueOnInstance(instance, value);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object extractedValue = extractFieldValueFromInstance(instance);
        if (referenceSavingResolver.shouldSaveReference(extractedValue, getAttributeValueContext())) {
            final Value<NamedResource> val = extractReferenceIdentifier(extractedValue);
            valueBuilder.addValue(createAssertion(), val, getAttributeWriteContext());
        } else {
            referenceSavingResolver
                    .registerPendingReference(valueBuilder.getSubjectIdentifier(), createAssertion(), extractedValue,
                                              getAttributeWriteContext());
            // This will cause the existing property assertion to be removed
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeWriteContext());
        }
    }

    private Set<URI> getAttributeValueContext() {
        return entityDescriptor.getAttributeDescriptor(attribute).getContexts();
    }

    private <V> Value<NamedResource> extractReferenceIdentifier(final V value) {
        if (value == null) {
            return Value.nullValue();
        }
        final Class<?> valueType = attribute.getJavaType();
        if (IdentifierTransformer.isValidIdentifierType(valueType)) {
            return new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(value)));
        } else if (valueType.isEnum()) {
            assert attribute.getConverter() != null;
            return new Value<>((NamedResource) attribute.getConverter().convertToAxiomValue(value));
        }
        final EntityType<? super V> valEt = (EntityType<? super V>) mapper.getEntityType(valueType);
        assert valEt != null;

        final URI id = EntityPropertiesUtils.getIdentifier(value, valEt);
        return id != null ? new Value<>(NamedResource.create(id)) : Value.nullValue();
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        final Object extractedValue = extractFieldValueFromInstance(instance);
        final Value<NamedResource> val = extractReferenceIdentifier(extractedValue);
        if (Value.nullValue().equals(val)) {
            return Collections.emptySet();
        }
        return Collections.singleton(
                new AxiomImpl<>(NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et)),
                                createAssertion(), val));
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
    }
}
