package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class TypesFieldStrategy extends FieldStrategy<TypesSpecification<?, ?>> {

	final Set<String> values;

	public TypesFieldStrategy(EntityType<?> et, TypesSpecification<?, ?> att,
			Descriptor descriptor, ObjectOntologyMapperImpl mapper) {
		super(et, att, descriptor, mapper);
		this.values = new HashSet<>();
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final String typeAsString = ax.getValue().getValue().toString();
		if (et.getIRI().toString().equals(typeAsString)) {
			return;
		}
		values.add(typeAsString);
	}

	@Override
	void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
			IllegalAccessException {
		checkFieldTypeCompatibility();
		if (values.isEmpty()) {
			return;
		}
		setValueOnInstance(instance, values);
	}

	private void checkFieldTypeCompatibility() {
		if (!attribute.getJavaField().getType().isAssignableFrom(values.getClass())) {
			throw new EntityReconstructionException(
					"The types field is not of a valid type. Expected Set<String>.");
		}
	}

	@Override
	Collection<Value<?>> extractAttributeValuesFromInstance(Object instance)
			throws IllegalArgumentException, IllegalAccessException {
		checkFieldTypeCompatibility();
		final Field typesField = attribute.getJavaField();
		if (!typesField.isAccessible()) {
			typesField.setAccessible(true);
		}
		// This cast is fine, it was checked by checkFieldCompatibility
		@SuppressWarnings("unchecked")
		final Set<String> types = (Set<String>) typesField.get(instance);
		if (types == null || types.isEmpty()) {
			return Collections.emptySet();
		}
		final Set<Value<?>> result = new HashSet<>(types.size());
		for (String type : types) {
			try {
				result.add(new Value<URI>(URI.create(type)));
			} catch (IllegalArgumentException e) {
				throw new EntityDeconstructionException("Type " + type + " is not a valid URI.", e);
			}
		}
		return result;
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createClassAssertion(attribute.isInferred());
	}
}
