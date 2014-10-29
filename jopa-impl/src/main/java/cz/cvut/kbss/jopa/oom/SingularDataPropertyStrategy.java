package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.exceptions.EntityReconstructionException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SingularDataPropertyStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

	private Object value;

	SingularDataPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att,
			Descriptor descriptor, EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final Value<?> val = ax.getValue();
		this.value = val.getValue();
	}

	@Override
	void buildInstanceFieldValue(Object entity) throws IllegalArgumentException,
			IllegalAccessException {
		final Field f = attribute.getJavaField();
		if (!f.isAccessible()) {
			f.setAccessible(true);
		}
		if (!f.getType().isAssignableFrom(value.getClass())) {
			throw new EntityReconstructionException("Incompatible types found. The field " + f
					+ " requires type " + f.getType() + ", but the loaded value is of type "
					+ value.getClass());
		}
		f.set(entity, value);
	}

	@Override
	void extractAttributeValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
			throws IllegalArgumentException, IllegalAccessException {
		final Object value = extractFieldValueFromInstance(instance);

		final Value<?> val = value != null ? new Value<>(value) : Value.nullValue();
		valueBuilder.addValue(createAssertion(), val, getAttributeContext());
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(),
				attribute.isInferred());
	}
}
