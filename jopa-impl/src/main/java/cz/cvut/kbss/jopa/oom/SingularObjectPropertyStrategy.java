package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class SingularObjectPropertyStrategy extends FieldStrategy {

	private Object value;

	SingularObjectPropertyStrategy(EntityType<?> et, Attribute<?, ?> att,
			ObjectOntologyMapper mapper) {
		super(et, att, mapper);
	}

	@Override
	void addValueFromAxiom(Axiom ax) {
		final Object valueIdentifier = ax.getValue().getValue();
		// TODO
		//this.value = mapper.getEntityFromCacheOrLoadItFromOntology
	}

	@Override
	void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
			IllegalAccessException {
		final Field f = attribute.getJavaField();
		f.setAccessible(true);
		// The type check was performed when the referenced value was being
		// loaded
		f.set(instance, value);
	}

}
