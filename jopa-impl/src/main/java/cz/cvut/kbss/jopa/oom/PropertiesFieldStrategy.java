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

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.exceptions.InvalidAssertionIdentifierException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class PropertiesFieldStrategy<X> extends
        FieldStrategy<PropertiesSpecification<? super X, ?>, X> {

    private final Map<String, Set<String>> values;

    PropertiesFieldStrategy(EntityType<X> et, PropertiesSpecification<? super X, ?> att,
                            Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
        this.values = new HashMap<>();
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final String property = ax.getAssertion().getIdentifier().toString();
        if (shouldSkipAxiom(ax)) {
            return;
        }
        if (!values.containsKey(property)) {
            values.put(property, new HashSet<>());
        }
        final String value = ax.getValue().stringValue();
        values.get(property).add(value);
    }

    private boolean shouldSkipAxiom(Axiom<?> ax) {
        final String property = ax.getAssertion().getIdentifier().toString();
        // This is class assertion for entities without types
        return property.equals(CommonVocabulary.RDF_TYPE) || isMappedAttribute(ax);
    }

    private boolean isMappedAttribute(Axiom<?> ax) {
        // TODO This is too simple, in case the assertion corresponds to a mapped attribute,
        // we also have to check whether the value is suitable for the attribute. If not, it belongs to properties
        final IRI propertyAsIri = IRI.create(ax.getAssertion().getIdentifier().toString());
        for (Attribute<?, ?> att : et.getAttributes()) {
            if (att.getIRI().equals(propertyAsIri)) {
                return true;
            }
        }
        return false;
    }

    @Override
    void buildInstanceFieldValue(Object instance) throws IllegalAccessException {
        if (values.isEmpty()) {
            return;
        }
        setValueOnInstance(instance, values);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        final Object val = extractFieldValueFromInstance(instance);
        final X original = mapper.getOriginalInstance(instance);
        if (val == null) {
            if (original == null) {
                return;
            }
            final Map<String, Set<String>> origProps = (Map<String, Set<String>>) extractFieldValueFromInstance(
                    original);
            if (origProps == null || origProps.isEmpty()) {
                return;
            }
            valueBuilder.removeProperties(prepareProperties(origProps), getAttributeContext());
            return;
        }
        assert val instanceof Map;
        final Map<String, Set<String>> props = (Map<String, Set<String>>) val;
        if (original == null) {
            valueBuilder.addProperties(prepareProperties(props), getAttributeContext());
        } else {
            final Map<String, Set<String>> origProps = (Map<String, Set<String>>) extractFieldValueFromInstance(
                    original);
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

    private Map<Assertion, Set<Value<?>>> prepareProperties(Map<String, Set<String>> props) {
        final Map<Assertion, Set<Value<?>>> result = new HashMap<>(props.size());
        for (Entry<String, Set<String>> entry : props.entrySet()) {
            result.put(stringToAssertion(entry.getKey()), stringsToValues(entry.getValue()));
        }
        return result;
    }

    private Assertion stringToAssertion(String property) {
        final Assertion assertion;
        try {
            assertion = Assertion.createPropertyAssertion(URI.create(property), attribute.isInferred());
        } catch (IllegalArgumentException e) {
            throw new InvalidAssertionIdentifierException(property + " is not a valid URI.", e);
        }
        return assertion;
    }

    private Set<Value<?>> stringsToValues(Set<String> strValues) {
        final Set<Value<?>> ontoValues = new HashSet<>(strValues.size());
        ontoValues.addAll(strValues.stream().map(Value::new).collect(Collectors.toList()));
        return ontoValues;
    }

    private Map<Assertion, Set<Value<?>>> resolvePropertiesToRemove(Map<String, Set<String>> current,
                                                                    Map<String, Set<String>> original) {
        return propertyDiff(original, current);
    }

    /**
     * The difference is counted as the properties and values which were in base but are not in the updated version.
     */
    private Map<Assertion, Set<Value<?>>> propertyDiff(Map<String, Set<String>> base,
                                                       Map<String, Set<String>> updated) {
        if (base == null || base.isEmpty()) {
            return Collections.emptyMap();
        }
        final Map<Assertion, Set<Value<?>>> diff = new HashMap<>();
        for (Entry<String, Set<String>> entry : base.entrySet()) {
            if (!updated.containsKey(entry.getKey())
                    || updated.get(entry.getKey()) == null || updated.get(entry.getKey()).isEmpty()) {
                // All values of the property are missing
                diff.put(stringToAssertion(entry.getKey()), stringsToValues(entry.getValue()));
            } else {
                final Set<String> currentValues = updated.get(entry.getKey());
                Set<String> removed = new HashSet<>();
                // Check which property values are missing
                removed.addAll(entry.getValue().stream().filter(origVal -> !currentValues.contains(origVal))
                                    .collect(Collectors.toList()));
                if (!removed.isEmpty()) {
                    diff.put(stringToAssertion(entry.getKey()), stringsToValues(removed));
                }
            }
        }
        return diff;
    }

    private Map<Assertion, Set<Value<?>>> resolvePropertiesToAdd(Map<String, Set<String>> current,
                                                                 Map<String, Set<String>> original) {
        return propertyDiff(current, original);
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createUnspecifiedPropertyAssertion(attribute.isInferred());
    }

}
