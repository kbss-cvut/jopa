package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.jopa.oom.exceptions.EntityReconstructionException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

// TODO The strategy for working with types has to be revisited. The problem is with updating types, 
// when we might delete the entity type or re-persist it into the wrong context
public class TypesFieldStrategy<X> extends FieldStrategy<TypesSpecification<? super X, ?>, X> {

	final Set<String> values;

	public TypesFieldStrategy(EntityType<X> et, TypesSpecification<? super X, ?> att,
			Descriptor descriptor, EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
		this.values = new HashSet<>();
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final String typeAsString = ax.getValue().stringValue();
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
	void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
			throws IllegalArgumentException, IllegalAccessException {
		final Object val = extractFieldValueFromInstance(instance);
		if (val == null) {
			valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
			return;
		}
		if (!(val instanceof Set)) {
			throw new EntityDeconstructionException(
					"The types field is not of a valid type. Expected Set<String>.");
		}
		final Set<?> types = (Set<?>) val;
		if (types.isEmpty()) {
			valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
			return;
		}
		final Set<Value<?>> result = new HashSet<>(types.size());
		for (Object type : types) {
			try {
				result.add(new Value<URI>(URI.create(type.toString())));
			} catch (IllegalArgumentException e) {
				throw new EntityDeconstructionException("Type " + type + " is not a valid URI.", e);
			}
		}
		valueBuilder.addValues(createAssertion(), result, getAttributeContext());
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createClassAssertion(attribute.isInferred());
	}
}
