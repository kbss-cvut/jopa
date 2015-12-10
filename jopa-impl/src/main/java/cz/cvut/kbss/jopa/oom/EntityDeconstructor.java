package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

class EntityDeconstructor {

	private final EntityMappingHelper mapper;
	private CascadeResolver cascadeResolver;

	EntityDeconstructor(ObjectOntologyMapperImpl mapper) {
		this.mapper = mapper;
	}

	void setCascadeResolver(CascadeResolver cascadeResolver) {
		this.cascadeResolver = cascadeResolver;
	}

	<T> AxiomValueGatherer mapEntityToAxioms(URI primaryKey, T entity, EntityType<T> et,
			Descriptor descriptor) {
		assert primaryKey != null;

		final AxiomValueGatherer valueBuilder = createAxiomValueBuilder(primaryKey, descriptor);
		try {
			addEntityClassAssertion(valueBuilder, entity, descriptor);
			if (et.getTypes() != null) {
				addAssertions(entity, et, et.getTypes(), descriptor, valueBuilder);
			}
			if (et.getProperties() != null) {
				addAssertions(entity, et, et.getProperties(), descriptor, valueBuilder);
			}
			for (Attribute<? super T, ?> att : et.getAttributes()) {
				addAssertions(entity, et, att, descriptor, valueBuilder);
			}

		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new EntityDeconstructionException(e);
		}
		return valueBuilder;
	}

	private AxiomValueGatherer createAxiomValueBuilder(URI primaryKey, Descriptor descriptor) {
		return new AxiomValueGatherer(NamedResource.create(primaryKey), descriptor.getContext());
	}

	private <T> void addEntityClassAssertion(AxiomValueGatherer valueBuilder, T entity,
			Descriptor descriptor) throws IllegalArgumentException, IllegalAccessException {
		final OWLClass clsType = entity.getClass().getAnnotation(OWLClass.class);
		assert clsType != null;
		final Assertion entityClassAssertion = Assertion.createClassAssertion(false);
		valueBuilder.addValue(entityClassAssertion, new Value<>(URI.create(clsType.iri())),
				descriptor.getContext());
	}

	private <T> void addAssertions(T entity, EntityType<T> et,
			FieldSpecification<? super T, ?> fieldSpec, Descriptor entityDescriptor,
			final AxiomValueGatherer valueBuilder) throws IllegalAccessException {
		final FieldStrategy<? extends FieldSpecification<? super T, ?>, T> fs = FieldStrategy
				.createFieldStrategy(et, fieldSpec, entityDescriptor.getAttributeDescriptor(fieldSpec), mapper);
		fs.setCascadeResolver(cascadeResolver);
		fs.buildAxiomValuesFromInstance(entity, valueBuilder);
	}

	<T> AxiomValueGatherer mapFieldToAxioms(URI primaryKey, T entity, Field field, EntityType<T> et,
			Descriptor descriptor) {
		final FieldSpecification<? super T, ?> fieldSpec = et
				.getFieldSpecification(field.getName());
		final AxiomValueGatherer valueBuilder = createAxiomValueBuilder(primaryKey, descriptor);
		try {
			addAssertions(entity, et, fieldSpec, descriptor, valueBuilder);
		} catch (IllegalAccessException e) {
			throw new EntityDeconstructionException(e);
		}
		return valueBuilder;
	}
}
