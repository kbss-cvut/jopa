package cz.cvut.kbss.jopa.oom;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.utils.CommonConstants;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class PropertiesFieldStrategy extends FieldStrategy<PropertiesSpecification<?, ?>> {

	private final Map<String, Set<String>> values;

	PropertiesFieldStrategy(EntityType<?> et, PropertiesSpecification<?, ?> att,
			Descriptor descriptor, ObjectOntologyMapperImpl mapper) {
		super(et, att, descriptor, mapper);
		this.values = new HashMap<>();
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final String property = ax.getAssertion().getIdentifier().toString();
		if (property.equals(CommonVocabulary.RDF_TYPE)) {
			// This is class assertion for entities without types
			return;
		}
		if (!values.containsKey(property)) {
			values.put(property, new HashSet<String>());
		}
		final String value = ax.getValue().stringValue();
		values.get(property).add(value);

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
	Collection<Value<?>> extractAttributeValuesFromInstance(Object instance)
			throws IllegalArgumentException, IllegalAccessException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createPropertyAssertion(CommonConstants.PROPERTIES_URI,
				attribute.isInferred());
	}

}
