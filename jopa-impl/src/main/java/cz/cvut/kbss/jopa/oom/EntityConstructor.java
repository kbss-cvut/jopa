package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.ontodriver_new.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class EntityConstructor {

	private final ObjectOntologyMapperImpl mapper;

	public EntityConstructor(ObjectOntologyMapperImpl mapper) {
		this.mapper = mapper;
	}

	<T> T reconstructEntity(Object primaryKey, EntityType<T> et, Descriptor descriptor,
			Collection<Axiom<?>> axioms) throws InstantiationException, IllegalAccessException {
		assert !axioms.isEmpty();
		final T instance = et.getJavaType().newInstance();
		mapper.setIdentifier(primaryKey, instance, et);
		mapper.registerInstance(primaryKey, instance, descriptor.getContext());
		final Map<URI, FieldSpecification<?, ?>> attributes = indexEntityAttributes(et);
		final Map<FieldSpecification<?, ?>, FieldStrategy<? extends FieldSpecification<?, ?>>> fieldLoaders = new HashMap<>(
				et.getAttributes().size() + 2);
		for (Axiom<?> ax : axioms) {
			final FieldStrategy<? extends FieldSpecification<?, ?>> fs = getFieldLoader(ax,
					attributes, fieldLoaders, et, descriptor);
			if (fs == null && isEntityClassAssertion(ax, et.getJavaType())) {
				continue;
			}
			assert fs != null;
			fs.addValueFromAxiom(ax);
		}
		for (FieldStrategy<?> fs : fieldLoaders.values()) {
			fs.buildInstanceFieldValue(instance);
		}
		return instance;
	}

	private Map<URI, FieldSpecification<?, ?>> indexEntityAttributes(EntityType<?> et) {
		final Map<URI, FieldSpecification<?, ?>> atts = new HashMap<>(et.getAttributes().size());
		for (Attribute<?, ?> at : et.getAttributes()) {
			atts.put(at.getIRI().toURI(), at);
		}
		if (et.getTypes() != null) {
			atts.put(URI.create(CommonVocabulary.RDF_TYPE), et.getTypes());
		}
		return atts;
	}

	private FieldStrategy<? extends FieldSpecification<?, ?>> getFieldLoader(
			Axiom<?> ax,
			Map<URI, FieldSpecification<?, ?>> attributes,
			Map<FieldSpecification<?, ?>, FieldStrategy<? extends FieldSpecification<?, ?>>> loaders,
			EntityType<?> et, Descriptor desc) {
		final URI attId = ax.getAssertion().getIdentifier();
		FieldSpecification<?, ?> att = attributes.get(attId);
		if (att == null) {
			if (et.getProperties() != null) {
				att = et.getProperties();
			} else {
				return null;
			}
		}
		if (!loaders.containsKey(att)) {
			loaders.put(att, FieldStrategy.createFieldStrategy(et, att,
					desc.getAttributeDescriptor(att), mapper));
		}
		return loaders.get(att);
	}

	private boolean isEntityClassAssertion(Axiom<?> ax, Class<?> cls) {
		return isClassAssertion(ax) && isEntityClass(ax, cls);
	}

	private boolean isClassAssertion(Axiom<?> ax) {
		return ax.getAssertion().getType() == AssertionType.CLASS;
	}

	private boolean isEntityClass(Axiom<?> ax, Class<?> cls) {
		final OWLClass clsAnn = cls.getAnnotation(OWLClass.class);
		assert clsAnn != null;
		final String val = ax.getValue().stringValue();
		return val.equals(clsAnn.iri());
	}

	<T> void setFieldValue(T entity, Field field, Collection<Axiom<?>> axioms, EntityType<T> et,
			Descriptor entityDescriptor) throws IllegalArgumentException, IllegalAccessException {
		final FieldSpecification<?, ?> fieldSpec = getFieldSpecification(field, et);
		final FieldStrategy<?> fs = FieldStrategy.createFieldStrategy(et, fieldSpec,
				entityDescriptor.getAttributeDescriptor(fieldSpec), mapper);
		for (Axiom<?> ax : axioms) {
			fs.addValueFromAxiom(ax);
		}
		fs.buildInstanceFieldValue(entity);
	}

	private FieldSpecification<?, ?> getFieldSpecification(Field field, EntityType<?> et) {
		if (et.getTypes() != null && et.getTypes().getJavaField().equals(field)) {
			return et.getTypes();
		} else if (et.getProperties() != null && et.getProperties().getJavaField().equals(field)) {
			return et.getProperties();
		} else {
			return et.getAttribute(field.getName());
		}
	}
}
