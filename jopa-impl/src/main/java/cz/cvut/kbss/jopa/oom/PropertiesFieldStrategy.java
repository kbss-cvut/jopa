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

import cz.cvut.kbss.jopa.exceptions.InvalidAssertionIdentifierException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;

class PropertiesFieldStrategy<X> extends FieldStrategy<PropertiesSpecification<? super X, ?, ?, ?>, X> {

    private final PropertiesValueHolder value = new PropertiesValueHolder();

    PropertiesFieldStrategy(EntityType<X> et, PropertiesSpecification<? super X, ?, ?, ?> att,
                            Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        if (shouldSkipAxiom(ax)) {
            return;
        }
        value.addValue(ax);
    }

    private boolean shouldSkipAxiom(Axiom<?> ax) {
        final String property = ax.getAssertion().getIdentifier().toString();
        // This is class assertion for entities without types
        return property.equals(RDF.TYPE) || isMappedAttribute(ax);
    }

    private boolean isMappedAttribute(Axiom<?> ax) {
        // TODO This is too simple, in case the assertion corresponds to a mapped attribute,
        // we also have to check whether the map is suitable for the attribute. If not, it belongs to properties
        final IRI propertyAsIri = IRI.create(ax.getAssertion().getIdentifier().toString());
        for (Attribute<?, ?> att : et.getAttributes()) {
            if (att.getIRI().equals(propertyAsIri)) {
                return true;
            }
        }
        return false;
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        if (value.getValue().isEmpty()) {
            return;
        }
        setValueOnInstance(instance, value.getValue());
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object val = extractFieldValueFromInstance(instance);
        final X original = mapper.getOriginalInstance(instance);
        if (val == null) {
            if (original == null) {
                return;
            }
            final Map<?, Set<?>> origProps = (Map<?, Set<?>>) extractFieldValueFromInstance(original);
            if (origProps == null || origProps.isEmpty()) {
                return;
            }
            valueBuilder.removeProperties(prepareProperties(origProps), getAttributeContext());
            return;
        }
        assert val instanceof Map;
        final Map<?, Set<?>> props = (Map<?, Set<?>>) val;
        if (original == null) {
            valueBuilder.addProperties(prepareProperties(props), getAttributeContext());
        } else {
            final Map<?, Set<?>> origProps = (Map<?, Set<?>>) extractFieldValueFromInstance(original);
            final Map<Assertion, Set<Value<?>>> toRemove = resolvePropertiesToRemove(props, origProps);
            if (!toRemove.isEmpty()) {
                valueBuilder.removeProperties(toRemove, getAttributeContext());
            }
            final Map<Assertion, Set<Value<?>>> toAdd = resolvePropertiesToAdd(props, origProps);
            if (!toAdd.isEmpty()) {
                valueBuilder.addProperties(toAdd, getAttributeContext());
            }
        }
    }

    private Map<Assertion, Set<Value<?>>> prepareProperties(Map<?, Set<?>> props) {
        final Map<Assertion, Set<Value<?>>> result = new HashMap<>(props.size());
        props.entrySet().stream().filter(e -> e.getKey() != null && e.getValue() != null)
             .forEach(e -> result.put(propertyToAssertion(e.getKey()), objectsToValues(e.getValue())));
        return result;
    }

    private Assertion propertyToAssertion(Object property) {
        try {
            return Assertion
                    .createPropertyAssertion(EntityPropertiesUtils.getValueAsURI(property), attribute.isInferred());
        } catch (IllegalArgumentException e) {
            throw new InvalidAssertionIdentifierException(property + " is not a valid identifier.", e);
        }
    }

    private static Set<Value<?>> objectsToValues(Collection<?> strValues) {
        final Set<Value<?>> ontoValues = new HashSet<>(strValues.size());
        ontoValues.addAll(strValues.stream().filter(Objects::nonNull).map(Value::new).collect(Collectors.toList()));
        return ontoValues;
    }

    private Map<Assertion, Set<Value<?>>> resolvePropertiesToRemove(Map<?, Set<?>> current, Map<?, Set<?>> original) {
        return propertyDiff(original, current);
    }

    /**
     * The difference is counted as the properties and values which were in base but are not in the updated version.
     */
    private Map<Assertion, Set<Value<?>>> propertyDiff(Map<?, Set<?>> base, Map<?, Set<?>> updated) {
        if (base == null || base.isEmpty()) {
            return Collections.emptyMap();
        }
        final Map<Assertion, Set<Value<?>>> diff = new HashMap<>();
        if (updated == null || updated.isEmpty()) {
            diff.putAll(createAssertionsForAll(base));
        } else {
            for (Entry<?, Set<?>> entry : base.entrySet()) {
                final Object key = entry.getKey();
                if (!updated.containsKey(key) || updated.get(key) == null || updated.get(key).isEmpty()) {
                    // All values of the property are missing
                    diff.put(propertyToAssertion(key), objectsToValues(entry.getValue()));
                } else {
                    final Set<?> currentValues = updated.get(key);
                    // Check which property values are missing
                    final List<?> removed =
                            entry.getValue().stream().filter(origVal -> !currentValues.contains(origVal))
                                 .collect(Collectors.toList());
                    if (!removed.isEmpty()) {
                        diff.put(propertyToAssertion(key), objectsToValues(removed));
                    }
                }
            }
        }
        return diff;
    }

    private Map<Assertion, Set<Value<?>>> createAssertionsForAll(Map<?, Set<?>> map) {
        final Map<Assertion, Set<Value<?>>> diff = new HashMap<>(map.size());
        map.forEach((key, val) -> diff.put(propertyToAssertion(key), objectsToValues(val)));
        return diff;
    }

    private Map<Assertion, Set<Value<?>>> resolvePropertiesToAdd(Map<?, Set<?>> current, Map<?, Set<?>> original) {
        return propertyDiff(current, original);
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createUnspecifiedPropertyAssertion(attribute.isInferred());
    }

    private class PropertiesValueHolder {

        private final Map<Object, Set<Object>> map = new HashMap<>();

        void addValue(Axiom<?> ax) {
            final Object property = mapPropertyIdentifier(ax.getAssertion());
            final Object val = mapPropertyValue(ax.getValue());
            if (!map.containsKey(property)) {
                map.put(property, new HashSet<>());
            }
            map.get(property).add(val);
        }

        private Object mapPropertyIdentifier(Assertion a) {
            final URI id = a.getIdentifier();
            final Class<?> propertyIdType = PropertiesFieldStrategy.this.attribute.getPropertyIdentifierType();
            assert IdentifierTransformer.isValidIdentifierType(propertyIdType);

            return IdentifierTransformer.transformToIdentifier(id, propertyIdType);
        }

        private Object mapPropertyValue(Value<?> value) {
            final Class<?> propertyValueType = PropertiesFieldStrategy.this.attribute.getPropertyValueType();
            Object val = value.getValue();
            if (val.getClass().equals(NamedResource.class)) {
                // Value is an object property value
                val = ((NamedResource) val).getIdentifier();
            }
            if (propertyValueType.isAssignableFrom(val.getClass())) {
                return val;
            } else {
                // String value type was always default
                if (propertyValueType.equals(String.class)) {
                    return val.toString();
                } else {
                    throw new IllegalArgumentException("Cannot return value " + val + " as type " + propertyValueType);
                }
            }
        }

        Map<Object, Set<Object>> getValue() {
            return map;
        }
    }
}
