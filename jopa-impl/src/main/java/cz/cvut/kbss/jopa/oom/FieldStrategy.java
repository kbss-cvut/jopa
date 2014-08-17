package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

abstract class FieldStrategy {

	final EntityType<?> et;
	final Attribute<?, ?> attribute;
	final ObjectOntologyMapper mapper;

	FieldStrategy(EntityType<?> et, Attribute<?, ?> att, ObjectOntologyMapper mapper) {
		this.et = et;
		this.attribute = att;
		this.mapper = mapper;
	}

	abstract void addValueFromAxiom(Axiom ax);

	abstract void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
			IllegalAccessException;

	static FieldStrategy createFieldStrategy(EntityType<?> et, Attribute<?, ?> att,
			ObjectOntologyMapper mapper) {
		// TODO
		if (att.isCollection()) {
			// TODO
			return null;
		} else {
			switch (att.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				return new SingularDataPropertyStrategy(et, att, mapper);
			case OBJECT:
				return null;
			}
		}
		// Shouldn't happen
		throw new IllegalArgumentException();
	}
}
