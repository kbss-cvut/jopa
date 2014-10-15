package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Map;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

abstract class FieldStrategy<T extends FieldSpecification<?, ?>> {

	final EntityType<?> et;
	final T attribute;
	final Descriptor descriptor;
	final EntityMappingHelper mapper;
	CascadeResolver cascadeResolver;

	FieldStrategy(EntityType<?> et, T att, Descriptor descriptor, EntityMappingHelper mapper) {
		this.et = et;
		this.attribute = att;
		this.descriptor = descriptor;
		this.mapper = mapper;
	}

	void setCascadeResolver(CascadeResolver resolver) {
		this.cascadeResolver = resolver;
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
		final Field field = attribute.getJavaField();
		if (!field.isAccessible()) {
			field.setAccessible(true);
		}
		field.set(instance, value);
	}

	/**
	 * Extracts the attribute value from the specified instance. </p>
	 * 
	 * @return Attribute value, possibly {@code null}
	 */
	Object extractFieldValueFromInstance(Object instance) throws IllegalArgumentException,
			IllegalAccessException {
		final Field field = attribute.getJavaField();
		if (!field.isAccessible()) {
			field.setAccessible(true);
		}
		return field.get(instance);
	}

	protected <E> URI resolveValueIdentifier(E instance, EntityType<E> valEt) {
		URI id = EntityPropertiesUtils.getPrimaryKey(instance, valEt);
		if (id == null) {
			id = mapper.generateIdentifier(valEt);
			EntityPropertiesUtils.setPrimaryKey(id, instance, valEt);
		}
		return id;
	}

	/**
	 * Checks that the field represented by this strategy is not inferred.
	 * 
	 * @throws OWLInferredAttributeModifiedException
	 *             If the attribute is indeed inferred
	 */
	void verifyAttributeIsNotInferred() {
		if (attribute.isInferred()) {
			throw new OWLInferredAttributeModifiedException("Cannot modify inferred attribute "
					+ attribute);
		}
	}

	/**
	 * Adds value from the specified axioms to this strategy. </p>
	 * 
	 * The value(s) is/are then set on entity field using
	 * {@link #buildInstanceFieldValue(Object)}.
	 * 
	 * @param ax
	 *            Axiom to extract value from
	 */
	abstract void addValueFromAxiom(Axiom<?> ax);

	/**
	 * Sets instance field from values gathered in this strategy.
	 * 
	 * @param instance
	 *            The instance to receive the field value
	 * @throws IllegalArgumentException
	 *             Access error
	 * @throws IllegalAccessException
	 *             Access error
	 */
	abstract void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
			IllegalAccessException;

	/**
	 * Extracts values of field represented by this strategy from the specified
	 * instance.
	 * 
	 * @param instance
	 *            The instance to extract values from
	 * @param V
	 *            Specifies type of the attribute value. Mostly usable for
	 *            object property values
	 * @return Map of values mapped by assertion, empty collection if there are
	 *         no values
	 * @throws IllegalArgumentException
	 *             Access error
	 * @throws IllegalAccessException
	 *             Access error
	 */
	abstract <V> Map<Assertion, Collection<Value<?>>> extractAttributeValuesFromInstance(
			Object instance) throws IllegalArgumentException, IllegalAccessException;

	/**
	 * Creates property assertion appropriate for the attribute represented by
	 * this strategy.
	 * 
	 * @return Property assertion
	 */
	abstract Assertion createAssertion();

	static FieldStrategy<? extends FieldSpecification<?, ?>> createFieldStrategy(EntityType<?> et,
			FieldSpecification<?, ?> att, Descriptor descriptor, EntityMappingHelper mapper) {
		if (att instanceof TypesSpecification<?, ?>) {
			return new TypesFieldStrategy(et, (TypesSpecification<?, ?>) att, descriptor, mapper);
		} else if (att instanceof PropertiesSpecification<?, ?>) {
			return new PropertiesFieldStrategy(et, (PropertiesSpecification<?, ?>) att, descriptor,
					mapper);
		}
		final Attribute<?, ?> attribute = (Attribute<?, ?>) att;
		if (attribute.isCollection()) {
			switch (attribute.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				throw new NotYetImplementedException();
			case OBJECT:
				return createPluralObjectPropertyStrategy(et, (PluralAttribute<?, ?, ?>) attribute,
						descriptor, mapper);
			default:
				break;
			}
		} else {
			switch (attribute.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				return new SingularDataPropertyStrategy(et, attribute, descriptor, mapper);
			case OBJECT:
				return new SingularObjectPropertyStrategy(et, attribute, descriptor, mapper);
			default:
				break;
			}
		}
		// Shouldn't happen
		throw new IllegalArgumentException();
	}

	private static FieldStrategy<? extends FieldSpecification<?, ?>> createPluralObjectPropertyStrategy(
			EntityType<?> et, PluralAttribute<?, ?, ?> attribute, Descriptor descriptor,
			EntityMappingHelper mapper) {
		switch (attribute.getCollectionType()) {
		case LIST:
			final ListAttribute<?, ?> listAtt = (ListAttribute<?, ?>) attribute;
			switch (listAtt.getSequenceType()) {
			case referenced:
				return new ReferencedListPropertyStrategy(et, listAtt, descriptor, mapper);
			case simple:
				return new SimpleListPropertyStrategy(et, listAtt, descriptor, mapper);
			default:
				throw new NotYetImplementedException("Unsupported list attribute sequence type "
						+ listAtt.getSequenceType());
			}
		case SET:
			return new PluralObjectPropertyStrategy(et, attribute, descriptor, mapper);
		default:
			throw new NotYetImplementedException("Unsupported plural attribute collection type "
					+ attribute.getCollectionType());
		}
	}
}
