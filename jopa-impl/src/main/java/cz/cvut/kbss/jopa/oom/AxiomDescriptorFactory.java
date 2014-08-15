package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class AxiomDescriptorFactory {

	private static final URI PROPERTIES_URI = URI.create("http://jopa.org/properties");

	AxiomDescriptor createForEntityLoading(URI primaryKey, Descriptor entityDescriptor,
			EntityType<?> et) {
		final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(primaryKey));
		descriptor.setSubjectContext(entityDescriptor.getContext());
		if (et.getTypes() != null && et.getTypes().getFetchType() != FetchType.LAZY) {
			final Assertion typesAssertion = Assertion.createClassAssertion(et.getTypes()
					.isInferred());
			addAssertionToDescriptor(entityDescriptor, et.getTypes(), descriptor, typesAssertion);
		}
		if (et.getProperties() != null && et.getProperties().getFetchType() != FetchType.LAZY) {
			final Assertion propsAssertion = Assertion.createPropertyAssertion(PROPERTIES_URI, et
					.getProperties().isInferred());
			addAssertionToDescriptor(entityDescriptor, et.getProperties(), descriptor,
					propsAssertion);
		}
		for (Attribute<?, ?> att : et.getAttributes()) {
			if (att.getFetchType() == FetchType.LAZY) {
				continue;
			}
			final Assertion a = createAssertion(att);
			addAssertionToDescriptor(entityDescriptor, att, descriptor, a);
		}
		return descriptor;
	}

	private void addAssertionToDescriptor(Descriptor entityDescriptor,
			FieldSpecification<?, ?> att, final AxiomDescriptor descriptor,
			final Assertion assertion) {
		descriptor.addAssertion(assertion);
		final URI typesContext = entityDescriptor.getAttributeDescriptor(att).getContext();
		if (typesContext != null) {
			descriptor.setAssertionContext(assertion, typesContext);
		}
	}

	private Assertion createAssertion(Attribute<?, ?> att) {
		assert att != null;
		switch (att.getPersistentAttributeType()) {
		case OBJECT:
			return Assertion.createObjectPropertyAssertion(att.getIRI().toURI(), att.isInferred());
		case DATA:
			return Assertion.createDataPropertyAssertion(att.getIRI().toURI(), att.isInferred());
		case ANNOTATION:
			return Assertion.createAnnotationPropertyAssertion(att.getIRI().toURI(),
					att.isInferred());
		}
		throw new IllegalArgumentException("Illegal persistent attribute type "
				+ att.getPersistentAttributeType());
	}

	AxiomDescriptor createForFieldLoading(URI primaryKey, Field field, Descriptor entityDescriptor,
			EntityType<?> et) {
		final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(primaryKey));
		FieldSpecification<?, ?> fieldSpec = getFieldSpecification(field, et);
		// TODO
		return descriptor;
	}

	private FieldSpecification<?, ?> getFieldSpecification(Field field, EntityType<?> et) {
		// TODO Auto-generated method stub
		return null;
	}
}
