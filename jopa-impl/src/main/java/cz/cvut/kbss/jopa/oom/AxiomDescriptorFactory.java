package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class AxiomDescriptorFactory {

	AxiomDescriptor createForEntityLoading(URI primaryKey, Descriptor entityDescriptor, EntityType<?> et) {
		// TODO
		final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(primaryKey));
		return descriptor;
	}
	
	AxiomDescriptor createForFieldLoading(URI primaryKey, Field field, Descriptor entityDescriptor, EntityType<?> et) {
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
