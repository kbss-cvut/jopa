package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.jopa.oom.exceptions.EntityReconstructionException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

// TODO Properties handling strategy has to be revisited. 
// What shall we do when merging properties or when the properties have been completely removed?
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
		if (property.equals(CommonVocabulary.RDF_TYPE)) {
			// This is class assertion for entities without types
			return true;
		}
		if (isMappedAttribute(ax.getAssertion().getIdentifier())) {
			// Mapped attribute values don't belong into properties
			return true;
		}
		return false;
	}

	private boolean isMappedAttribute(URI property) {
		final IRI propertyAsIri = IRI.create(property.toString());
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
		checkFieldCompatibility();
		if (values.isEmpty()) {
			return;
		}
		setValueOnInstance(instance, values);
	}

	private void checkFieldCompatibility() {
		if (!attribute.getJavaField().getType().isAssignableFrom(values.getClass())) {
			throw new EntityReconstructionException(
					"The properties field is not of a valid type. Expected Map<String, Set<String>>.");
		}
	}

	@Override
	void extractAttributeValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
			throws IllegalArgumentException, IllegalAccessException {
		final Object val = extractFieldValueFromInstance(instance);
		if (val == null) {
			// TODO What shall we do here?
			return;
		}
		if (!(val instanceof Map)) {
			throw new EntityDeconstructionException("The properties field has to be a map!");
		}
		final Map<?, ?> props = (Map<?, ?>) val;
		if (props.isEmpty()) {
			// TODO
			return;
		}
		for (Entry<?, ?> e : props.entrySet()) {
			if (e.getValue() == null) {
				continue;
			}
			if (!(e.getValue() instanceof Set)) {
				throw new EntityDeconstructionException(
						"The properties map value element has to be a set!");
			}
			final Assertion assertion = Assertion.createPropertyAssertion(
					URI.create(e.getKey().toString()), attribute.isInferred());
			final Set<?> propertyValue = (Set<?>) e.getValue();
			final Set<Value<?>> values = new HashSet<>(propertyValue.size());
			for (Object v : propertyValue) {
				values.add(new Value<>(v));
			}
			valueBuilder.addValues(assertion, values, getAttributeContext());
		}
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createUnspecifiedPropertyAssertion(attribute.isInferred());
	}

}
