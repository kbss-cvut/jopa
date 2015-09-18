package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.exceptions.InvalidAssertionIdentifierException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

import java.net.URI;
import java.util.*;
import java.util.Map.Entry;

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
            values.put(property, new HashSet<String>());
        }
        final String value = ax.getValue().stringValue();
        values.get(property).add(value);
    }

    private boolean shouldSkipAxiom(Axiom<?> ax) {
        final String property = ax.getAssertion().getIdentifier().toString();
        // This is class assertion for entities without types
        return property.equals(CommonVocabulary.RDF_TYPE)
                || isMappedAttribute(ax);
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
    void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
            IllegalAccessException {
        if (values.isEmpty()) {
            return;
        }
        setValueOnInstance(instance, values);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
            throws IllegalArgumentException, IllegalAccessException {
        final Object val = extractFieldValueFromInstance(instance);
        final X original = mapper.getOriginalInstance(instance);
        if (val == null) {
            if (original == null) {
                return;
            }
            final Map<String, Set<String>> origProps = (Map<String, Set<String>>) extractFieldValueFromInstance(original);
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
            final Map<String, Set<String>> origProps = (Map<String, Set<String>>) extractFieldValueFromInstance(original);
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
        for (String strVal : strValues) {
            ontoValues.add(new Value<>(strVal));
        }
        return ontoValues;
    }

    private Map<Assertion, Set<Value<?>>> resolvePropertiesToRemove(Map<String, Set<String>> current,
                                                                    Map<String, Set<String>> original) {
        if (original == null || original.isEmpty()) {
            return Collections.emptyMap();
        }
        return propertyDiff(current, original);
    }

    private Map<Assertion, Set<Value<?>>> propertyDiff(Map<String, Set<String>> current, Map<String, Set<String>> original) {
        final Map<Assertion, Set<Value<?>>> diff = new HashMap<>();
        for (Entry<String, Set<String>> entry : original.entrySet()) {
            if (!current.containsKey(entry.getKey())
                    || current.get(entry.getKey()) == null || current.get(entry.getKey()).isEmpty()) {
                // All values of the property are missing
                diff.put(stringToAssertion(entry.getKey()), stringsToValues(entry.getValue()));
            } else {
                final Set<String> currentValues = current.get(entry.getKey());
                Set<String> removed = new HashSet<>();
                // Check which property values are missing
                for (String origVal : entry.getValue()) {
                    if (!currentValues.contains(origVal)) {
                        removed.add(origVal);
                    }
                }
                if (!removed.isEmpty()) {
                    diff.put(stringToAssertion(entry.getKey()), stringsToValues(removed));
                }
            }
        }
        return diff;
    }

    private Map<Assertion, Set<Value<?>>> resolvePropertiesToAdd(Map<String, Set<String>> current,
                                                                 Map<String, Set<String>> original) {
        if (original == null || original.isEmpty()) {
            return Collections.emptyMap();
        }
        return propertyDiff(original, current);
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createUnspecifiedPropertyAssertion(attribute.isInferred());
    }

}
