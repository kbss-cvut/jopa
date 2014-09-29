package cz.cvut.kbss.jopa.oom;

import java.net.URI;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;

interface EntityMappingHelper {
	
	<T> T getEntityFromCacheOrOntology(Class<T> cls, URI primaryKey, Descriptor descriptor);

	<T> EntityType<T> getEntityType(Class<T> cls);
	
	URI generateIdentifier(EntityType<?> et);
}
