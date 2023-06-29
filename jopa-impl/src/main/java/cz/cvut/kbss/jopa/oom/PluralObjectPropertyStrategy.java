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
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

abstract class PluralObjectPropertyStrategy<Y extends AbstractPluralAttribute<? super X, ?, ?>, X>
        extends FieldStrategy<Y, X> {

    private static final Logger LOG = LoggerFactory.getLogger(PluralObjectPropertyStrategy.class);

    private final Collection<Object> values;

    PluralObjectPropertyStrategy(EntityType<X> et, Y att, Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
        this.values = CollectionFactory.createDefaultCollection(att.getCollectionType());
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final NamedResource valueIdentifier = (NamedResource) ax.getValue().getValue();
        final Class<?> elementType = attribute.getBindableJavaType();
        if (IdentifierTransformer.isValidIdentifierType(elementType)) {
            values.add(IdentifierTransformer.transformToIdentifier(valueIdentifier.getIdentifier(), elementType));
        } else if (elementType.isEnum()) {
            assert attribute.getConverter() != null;
            assert attribute.getConverter().supportsAxiomValueType(NamedResource.class);
            // Maybe we should catch the exception and just skip the value if it is not convertible to be consistent
            // with the 'else' branch behavior?
            values.add(attribute.getConverter().convertToAttribute(valueIdentifier));
        } else {
            final Object value = mapper.getEntityFromCacheOrOntology(elementType, valueIdentifier.getIdentifier(),
                                                                     entityDescriptor.getAttributeDescriptor(
                                                                             attribute));
            if (value != null) {
                values.add(value);
            } else {
                LOG.trace("Value of axiom {} could not be loaded as entity filling attribute {}.", ax, attribute);
            }
        }
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        if (!values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            return Collections.emptySet();
        }
        final NamedResource subject = NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et));
        final Assertion assertion = createAssertion();
        final Function<Object, Value<NamedResource>> valueMapper = resolveValueMapper();
        return valueCollection.stream().filter(Objects::nonNull)
                              .map(valueMapper)
                              .map(v -> new AxiomImpl<>(subject, assertion, v)).collect(Collectors.toSet());
    }

    private Function<Object, Value<NamedResource>> resolveValueMapper() {
        final Class<?> type = attribute.getBindableJavaType();
        if (IdentifierTransformer.isValidIdentifierType(type)) {
            return v -> new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(v)));
        } else if (type.isEnum()) {
            assert attribute.getConverter() != null;
            return v -> new Value<>((NamedResource) attribute.getConverter().convertToAxiomValue(v));
        } else {
            final EntityType<?> et = mapper.getEntityType(type);
            return v -> new Value<>(NamedResource.create(EntityPropertiesUtils.getIdentifier(v, et)));
        }
    }

    @Override
    Collection<Value<?>> toAxiomValue(Object value) {
        return Collections.singleton(resolveValueMapper().apply(value));
    }

    /**
     * Gets the context(s) in which this attribute values are stored.
     * <p>
     * I.e., these contexts may (and usually will be) different from the context in which this attribute's property
     * assertion is stored, since that is usually stored in the subject's context.
     *
     * @return Attribute value (referenced entity) context
     * @see Descriptor#areAssertionsInSubjectContext()
     */
    Set<URI> getAttributeValueContexts() {
        return entityDescriptor.getAttributeDescriptor(attribute).getContexts();
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
    }
}
