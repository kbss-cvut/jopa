package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collection;
import java.util.Map;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
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

	<T> MutationAxiomDescriptor mapEntityToAxioms(URI primaryKey, T entity, EntityType<T> et,
			Descriptor descriptor) {
		assert primaryKey != null;

		final MutationAxiomDescriptor axiomDescriptor = createAxiomDescriptor(primaryKey,
				descriptor.getContext());
		try {
			addEntityClassAssertion(axiomDescriptor, entity, descriptor);
			if (et.getTypes() != null) {
				addAssertions(entity, et, et.getTypes(), descriptor, axiomDescriptor);
			}
			if (et.getProperties() != null) {
				addAssertions(entity, et, et.getProperties(), descriptor, axiomDescriptor);
			}
			for (Attribute<?, ?> att : et.getAttributes()) {
				addAssertions(entity, et, att, descriptor, axiomDescriptor);
			}

		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new EntityDeconstructionException(e);
		}
		return axiomDescriptor;
	}

	private MutationAxiomDescriptor createAxiomDescriptor(URI primaryKey, URI context) {
		final MutationAxiomDescriptor axiomDescriptor = new MutationAxiomDescriptor(
				NamedResource.create(primaryKey));
		axiomDescriptor.setSubjectContext(context);
		return axiomDescriptor;
	}

	private <T> void addEntityClassAssertion(MutationAxiomDescriptor axiomDescriptor, T entity,
			Descriptor descriptor) throws IllegalArgumentException, IllegalAccessException {
		final OWLClass clsType = entity.getClass().getAnnotation(OWLClass.class);
		assert clsType != null;
		final Assertion entityClassAssertion = Assertion.createClassAssertion(false);
		axiomDescriptor.addAssertionValue(entityClassAssertion,
				new Value<URI>(URI.create(clsType.iri())));
		axiomDescriptor.setAssertionContext(entityClassAssertion, descriptor.getContext());
	}

	private <T> void addAssertions(T entity, EntityType<?> et, FieldSpecification<?, ?> fieldSpec,
			Descriptor descriptor, final MutationAxiomDescriptor axiomDescriptor)
			throws IllegalAccessException {
		final FieldStrategy<? extends FieldSpecification<?, ?>> fs = FieldStrategy
				.createFieldStrategy(et, fieldSpec, descriptor, mapper);
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

	private void setAssertionContext(MutationAxiomDescriptor axiomDescriptor,
			Descriptor descriptor, FieldSpecification<?, ?> att, final Assertion propertyAssertion) {
		final URI attContext = descriptor.getAttributeDescriptor(att).getContext();
		if (attContext != null) {
			axiomDescriptor.setAssertionContext(propertyAssertion, attContext);
		}
	}
}
