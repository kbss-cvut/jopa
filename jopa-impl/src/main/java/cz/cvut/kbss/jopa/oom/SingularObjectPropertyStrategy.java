package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SingularObjectPropertyStrategy extends FieldStrategy {

	private Object value;

	SingularObjectPropertyStrategy(EntityType<?> et, Attribute<?, ?> att, Descriptor descriptor,
			ObjectOntologyMapperImpl mapper) {
		super(et, att, descriptor, mapper);
	}

	@Override
	void addValueFromAxiom(Axiom ax) {
		// TODO Check that this cast is OK
		final URI valueIdentifier = (URI) ax.getValue().getValue();
		this.value = mapper.getEntityFromCacheOrOntology(attribute.getJavaType(), valueIdentifier,
				descriptor);
	}

	@Override
	void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
			IllegalAccessException {
		setValueOnInstance(instance, value);
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
		final EntityType<?> valEt = mapper.getEntityType(value.getClass());
		if (valEt == null) {
			throw new EntityDeconstructionException("Value of field " + field
					+ " is not a recognized entity.");
		}
		final Field idField = valEt.getIdentifier().getJavaField();
		if (!idField.isAccessible()) {
			idField.setAccessible(true);
		}
		final Object id = idField.get(value);
		return Collections.<Value<?>> singleton(new Value<>(id));
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(),
				attribute.isInferred());
	}
}
