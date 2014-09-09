package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SingularObjectPropertyStrategy extends FieldStrategy<Attribute<?, ?>> {

	private Object value;

	SingularObjectPropertyStrategy(EntityType<?> et, Attribute<?, ?> att, Descriptor descriptor,
			ObjectOntologyMapperImpl mapper) {
		super(et, att, descriptor, mapper);
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
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
	Map<Assertion, Collection<Value<?>>> extractAttributeValuesFromInstance(Object instance)
			throws IllegalArgumentException, IllegalAccessException {
		final Object value = extractFieldValueFromInstance(instance);
		if (value == null) {
			return Collections.emptyMap();
		}
		final EntityType<?> valEt = mapper.getEntityType(value.getClass());
		if (valEt == null) {
			throw new EntityDeconstructionException("Value of field " + attribute.getJavaField()
					+ " is not a recognized entity.");
		}
		final Field idField = valEt.getIdentifier().getJavaField();
		if (!idField.isAccessible()) {
			idField.setAccessible(true);
		}
		final Object id = idField.get(value);
		return Collections.<Assertion, Collection<Value<?>>> singletonMap(createAssertion(),
				Collections.<Value<?>> singleton(new Value<>(id)));
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(),
				attribute.isInferred());
	}
}
