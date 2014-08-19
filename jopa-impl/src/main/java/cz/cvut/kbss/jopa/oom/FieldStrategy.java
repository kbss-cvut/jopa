package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
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

	/**
	 * Sets the specified value on the specified instance, the field is taken
	 * from the attribute represented by this strategy. </p>
	 * 
	 * Note that this method assumes the value and the field are of compatible
	 * types, no check is done here.
	 */
	void setValueOnInstance(Object instance, Object value) throws IllegalArgumentException,
			IllegalAccessException {
		final Field f = attribute.getJavaField();
		f.setAccessible(true);
		f.set(instance, value);
	}

	abstract void addValueFromAxiom(Axiom ax);

	abstract void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
			IllegalAccessException;

	static FieldStrategy createFieldStrategy(EntityType<?> et, Attribute<?, ?> att,
			Descriptor descriptor, ObjectOntologyMapperImpl mapper) {
		if (att.isCollection()) {
			switch (att.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				throw new NotYetImplementedException();
			case OBJECT:
				return new PluralObjectPropertyStrategy(et, att, descriptor, mapper);
			default:
				break;
			}
		} else {
			switch (att.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				return new SingularDataPropertyStrategy(et, att, descriptor, mapper);
			case OBJECT:
				return new SingularObjectPropertyStrategy(et, att, descriptor, mapper);
			default:
				break;
			}
		}
		// Shouldn't happen
		throw new IllegalArgumentException();
	}
}
