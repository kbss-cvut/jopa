/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;

class SingularObjectPropertyStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

    private static final Logger LOG = LoggerFactory.getLogger(SingularObjectPropertyStrategy.class);

    private Object value;

    SingularObjectPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att,
                                   Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        assert ax.getValue().getValue() instanceof NamedResource;
        final NamedResource valueIdentifier = (NamedResource) ax.getValue().getValue();
        final Object newValue;
        if (IdentifierTransformer.isValidIdentifierType(attribute.getJavaType())) {
            newValue = IdentifierTransformer
                    .transformToIdentifier(valueIdentifier.getIdentifier(), attribute.getJavaType());
        } else {
            newValue = mapper
                    .getEntityFromCacheOrOntology(attribute.getJavaType(), valueIdentifier.getIdentifier(),
                            attributeDescriptor);
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
        if (referenceSavingResolver.shouldSaveReference(extractedValue, getAttributeContext())) {
            final Value<NamedResource> val = extractReferenceIdentifier(extractedValue);
            valueBuilder.addValue(createAssertion(), val, getAttributeContext());
        } else {
            referenceSavingResolver
                    .registerPendingReference(valueBuilder.getSubjectIdentifier(), createAssertion(), extractedValue,
                            getAttributeContext());
            // This will cause the existing property assertion to be removed
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
        }
    }

    private <V> Value<NamedResource> extractReferenceIdentifier(final V value) {
        if (value == null) {
            return Value.nullValue();
        }
        if (IdentifierTransformer.isValidIdentifierType(attribute.getJavaType())) {
            return new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(value)));
        }
        final EntityType<? super V> valEt = (EntityType<? super V>) mapper.getEntityType(attribute.getJavaType());
        assert valEt != null;

        final URI id = EntityPropertiesUtils.getIdentifier(value, valEt);
        return id != null ? new Value<>(NamedResource.create(id)) : Value.nullValue();
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
    }
}
