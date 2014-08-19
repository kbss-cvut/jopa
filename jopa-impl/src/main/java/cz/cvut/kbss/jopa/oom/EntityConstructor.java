package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.ontodriver.exceptions.UnassignableIdentifierException;
import cz.cvut.kbss.ontodriver_new.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class EntityConstructor {

	private final ObjectOntologyMapperImpl mapper;

	public EntityConstructor(ObjectOntologyMapperImpl mapper) {
		this.mapper = mapper;
	}

	<T> T reconstructEntity(Object primaryKey, EntityType<T> et, Descriptor descriptor,
			Collection<Axiom> axioms) throws InstantiationException, IllegalAccessException {
		assert !axioms.isEmpty();
		final T instance = et.getJavaType().newInstance();
		setIdentifier(primaryKey, instance, et);
		mapper.registerInstance(primaryKey, instance, descriptor.getContext());
		final Set<String> types = new HashSet<>();
		final Map<String, String> properties = new HashMap<>();
		final Map<URI, Attribute<?, ?>> attributes = indexEntityAttributes(et);
		final Map<Attribute<?, ?>, FieldStrategy> fieldLoaders = new HashMap<>(et.getAttributes()
				.size());
		for (Axiom ax : axioms) {
			if (isClassAssertion(ax)) {
				if (!isEntityClass(ax, et.getJavaType())) {
					types.add(ax.getValue().toString());
				}
			} else if (!attributes.containsKey(ax.getAssertion().getIdentifier())) {
				properties.put(ax.getAssertion().getIdentifier().toString(), ax.getValue()
						.toString());
			} else {
				final FieldStrategy fs = getFieldLoader(ax, attributes, fieldLoaders, et,
						descriptor);
				fs.addValueFromAxiom(ax);
			}
		}
		if (et.getTypes() != null && !types.isEmpty()) {
			setFieldValue(et.getTypes(), instance, types);
		}
		if (et.getProperties() != null && !properties.isEmpty()) {
			setFieldValue(et.getProperties(), instance, properties);
		}
		for (FieldStrategy fs : fieldLoaders.values()) {
			fs.buildInstanceFieldValue(instance);
		}
		return instance;
	}

	private <T> void setIdentifier(Object identifier, T instance, EntityType<T> et)
			throws IllegalArgumentException, IllegalAccessException {
		final Identifier id = et.getIdentifier();
		final Field idField = id.getJavaField();
		if (!idField.getType().isAssignableFrom(identifier.getClass())) {
			throw new UnassignableIdentifierException("Cannot assign identifier of type "
					+ identifier + " to field of type " + idField.getType());
		}
		idField.setAccessible(true);
		idField.set(instance, identifier);
	}

	private Map<URI, Attribute<?, ?>> indexEntityAttributes(EntityType<?> et) {
		final Map<URI, Attribute<?, ?>> atts = new HashMap<>(et.getAttributes().size());
		for (Attribute<?, ?> at : et.getAttributes()) {
			atts.put(at.getIRI().toURI(), at);
		}
		return atts;
	}

	private boolean isClassAssertion(Axiom ax) {
		return ax.getAssertion().getType() == AssertionType.CLASS;
	}

	private boolean isEntityClass(Axiom ax, Class<?> cls) {
		final OWLClass clsAnn = cls.getAnnotation(OWLClass.class);
		assert clsAnn != null;
		final String val = ax.getValue().toString();
		return val.equals(clsAnn.iri());
	}

	private FieldStrategy getFieldLoader(Axiom ax, Map<URI, Attribute<?, ?>> attributes,
			Map<Attribute<?, ?>, FieldStrategy> loaders, EntityType<?> et, Descriptor desc) {
		final URI attId = ax.getAssertion().getIdentifier();
		final Attribute<?, ?> att = attributes.get(attId);
		if (!loaders.containsKey(att)) {
			loaders.put(attributes.get(attId), FieldStrategy.createFieldStrategy(et, att,
					desc.getAttributeDescriptor(att), mapper));
		}
		return loaders.get(att);
	}

	private void setFieldValue(FieldSpecification<?, ?> fieldSpec, Object instance, Object value)
			throws IllegalArgumentException, IllegalAccessException {
		final Field field = fieldSpec.getJavaField();
		field.setAccessible(true);
		field.set(instance, value);
	}

	<T> void setFieldValue(T entity, Field field, Collection<Axiom> axioms, EntityType<T> et) {
		// TODO
	}
}
