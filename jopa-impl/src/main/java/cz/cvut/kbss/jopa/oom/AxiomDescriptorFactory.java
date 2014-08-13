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

	AxiomDescriptor createForEntityLoading(URI primaryKey, Descriptor entityDescriptor,
			EntityType<?> et) {
		final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(primaryKey));
		if (et.getTypes() != null && et.getTypes().getFetchType() != FetchType.LAZY) {
			final Assertion typesAssertion = Assertion.createClassAssertion(et.getTypes()
					.isInferred());
			descriptor.addAssertion(typesAssertion);
			final URI typesContext = entityDescriptor.getAttributeDescriptor(et.getTypes())
					.getContext();
			if (typesContext != null) {
				descriptor.setAssertionContext(typesAssertion, typesContext);
			}
		}
		// TODO
		for (Attribute<?, ?> att : et.getAttributes()) {
			if (att.getFetchType() == FetchType.LAZY) {
				continue;
			}
			final Assertion a = createAssertion(att);
			descriptor.addAssertion(a);
			final URI attContext = entityDescriptor.getAttributeDescriptor(att).getContext();
			if (attContext != null) {
				descriptor.setAssertionContext(a, attContext);
			}
		}
		return descriptor;
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
