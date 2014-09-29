package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.exceptions.EntityReconstructionException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SingularDataPropertyStrategy extends FieldStrategy<Attribute<?, ?>> {

	private Object value;

	SingularDataPropertyStrategy(EntityType<?> et, Attribute<?, ?> att, Descriptor descriptor,
			EntityMappingHelper mapper) {
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
		f.setAccessible(true);
		if (!f.getType().isAssignableFrom(value.getClass())) {
			throw new EntityReconstructionException("Incompatible types found. The field " + f
					+ " requires type " + f.getType() + ", but the loaded value is of type "
					+ value.getClass());
		}
		f.set(entity, value);
	}

	@Override
	Map<Assertion, Collection<Value<?>>> extractAttributeValuesFromInstance(Object instance)
			throws IllegalArgumentException, IllegalAccessException {
		final Object value = extractFieldValueFromInstance(instance);
		if (value == null) {
			return Collections.emptyMap();
		}
		final Value<?> val = new Value<>(value);
		return Collections.<Assertion, Collection<Value<?>>> singletonMap(createAssertion(),
				Collections.<Value<?>> singleton(val));
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(),
				attribute.isInferred());
	}
}
