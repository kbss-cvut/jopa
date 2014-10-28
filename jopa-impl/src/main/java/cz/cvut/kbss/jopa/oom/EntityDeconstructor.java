package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Map;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

class EntityDeconstructor {

	private final EntityMappingHelper mapper;
	private CascadeResolver cascadeResolver;

	EntityDeconstructor(ObjectOntologyMapperImpl mapper) {
		this.mapper = mapper;
	}

	void setCascadeResolver(CascadeResolver cascadeResolver) {
		this.cascadeResolver = cascadeResolver;
	}

	<T> AxiomValueDescriptor mapEntityToAxioms(URI primaryKey, T entity, EntityType<T> et,
			Descriptor descriptor) {
		assert primaryKey != null;

		final AxiomValueDescriptor axiomDescriptor = createAxiomDescriptor(primaryKey,
				descriptor.getContext());
		try {
			addEntityClassAssertion(axiomDescriptor, entity, descriptor);
			if (et.getTypes() != null) {
				addAssertions(entity, et, et.getTypes(), descriptor, axiomDescriptor);
			}
			if (et.getProperties() != null) {
				addAssertions(entity, et, et.getProperties(), descriptor, axiomDescriptor);
			}
			for (Attribute<? super T, ?> att : et.getAttributes()) {
				addAssertions(entity, et, att, descriptor, axiomDescriptor);
			}

		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new EntityDeconstructionException(e);
		}
		return axiomDescriptor;
	}

	private AxiomValueDescriptor createAxiomDescriptor(URI primaryKey, URI context) {
		final AxiomValueDescriptor axiomDescriptor = new AxiomValueDescriptor(
				NamedResource.create(primaryKey));
		axiomDescriptor.setSubjectContext(context);
		return axiomDescriptor;
	}

	private <T> void addEntityClassAssertion(AxiomValueDescriptor axiomDescriptor, T entity,
			Descriptor descriptor) throws IllegalArgumentException, IllegalAccessException {
		final OWLClass clsType = entity.getClass().getAnnotation(OWLClass.class);
		assert clsType != null;
		final Assertion entityClassAssertion = Assertion.createClassAssertion(false);
		axiomDescriptor.addAssertionValue(entityClassAssertion,
				new Value<URI>(URI.create(clsType.iri())));
		axiomDescriptor.setAssertionContext(entityClassAssertion, descriptor.getContext());
	}

	private <T> void addAssertions(T entity, EntityType<T> et,
			FieldSpecification<? super T, ?> fieldSpec, Descriptor descriptor,
			final AxiomValueDescriptor axiomDescriptor) throws IllegalAccessException {
		final FieldStrategy<? extends FieldSpecification<? super T, ?>, T> fs = FieldStrategy
				.createFieldStrategy(et, fieldSpec, descriptor, mapper);
		// fs.verifyAttributeIsNotInferred();
		fs.setCascadeResolver(cascadeResolver);
		final Map<Assertion, Collection<Value<?>>> values = fs
				.extractAttributeValuesFromInstance(entity);
		for (Assertion assertion : values.keySet()) {
			axiomDescriptor.addAssertion(assertion);
			for (Value<?> v : values.get(assertion)) {
				axiomDescriptor.addAssertionValue(assertion, v);
			}
			setAssertionContext(axiomDescriptor, descriptor, fieldSpec, assertion);
		}
	}

	private void setAssertionContext(AxiomValueDescriptor axiomDescriptor, Descriptor descriptor,
			FieldSpecification<?, ?> att, final Assertion propertyAssertion) {
		final URI attContext = descriptor.getAttributeDescriptor(att).getContext();
		if (attContext != null) {
			axiomDescriptor.setAssertionContext(propertyAssertion, attContext);
		}
	}

	<T> AxiomValueDescriptor mapFieldToAxioms(URI primaryKey, T entity, Field field,
			EntityType<T> et, Descriptor descriptor) {
		final AxiomValueDescriptor axiomDescriptor = createAxiomDescriptor(primaryKey,
				descriptor.getContext());
		final FieldSpecification<? super T, ?> fieldSpec = et
				.getFieldSpecification(field.getName());
		try {
			addAssertions(entity, et, fieldSpec, descriptor, axiomDescriptor);
		} catch (IllegalAccessException e) {
			throw new EntityDeconstructionException(e);
		}
		return axiomDescriptor;
	}
}
