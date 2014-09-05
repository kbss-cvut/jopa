package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SingularDataPropertyStrategy extends FieldStrategy<Attribute<?, ?>> {

	private Object value;

	SingularDataPropertyStrategy(EntityType<?> et, Attribute<?, ?> att, Descriptor descriptor,
			ObjectOntologyMapperImpl mapper) {
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
	Collection<Value<?>> extractAttributeValuesFromInstance(Object instance)
			throws IllegalArgumentException, IllegalAccessException {
		final Field field = attribute.getJavaField();
		if (!field.isAccessible()) {
			field.setAccessible(true);
		}
		final Object value = field.get(instance);
		if (value == null) {
			return Collections.emptySet();
		}
		final Value<?> val = new Value<>(value);
		return Collections.<Value<?>> singleton(val);
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(),
				attribute.isInferred());
	}
}
