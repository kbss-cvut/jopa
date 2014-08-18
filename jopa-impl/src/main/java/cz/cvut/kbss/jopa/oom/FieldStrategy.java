package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

abstract class FieldStrategy {

	final EntityType<?> et;
	final Attribute<?, ?> attribute;
	final Descriptor descriptor;
	final ObjectOntologyMapperImpl mapper;

	FieldStrategy(EntityType<?> et, Attribute<?, ?> att, Descriptor descriptor,
			ObjectOntologyMapperImpl mapper) {
		this.et = et;
		this.attribute = att;
		this.descriptor = descriptor;
		this.mapper = mapper;
	}

	abstract void addValueFromAxiom(Axiom ax);

	abstract void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
			IllegalAccessException;

	static FieldStrategy createFieldStrategy(EntityType<?> et, Attribute<?, ?> att,
			Descriptor descriptor, ObjectOntologyMapperImpl mapper) {
		// TODO
		if (att.isCollection()) {
			// TODO
			return null;
		} else {
			switch (att.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				return new SingularDataPropertyStrategy(et, att, descriptor, mapper);
			case OBJECT:
				return new SingularObjectPropertyStrategy(et, att, descriptor, mapper);
			}
		}
		// Shouldn't happen
		throw new IllegalArgumentException();
	}
}
