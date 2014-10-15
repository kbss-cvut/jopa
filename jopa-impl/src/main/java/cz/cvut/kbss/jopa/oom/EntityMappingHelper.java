package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collection;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

interface EntityMappingHelper {

	<T> T getEntityFromCacheOrOntology(Class<T> cls, URI primaryKey, Descriptor descriptor);

	<T> EntityType<T> getEntityType(Class<T> cls);

	URI generateIdentifier(EntityType<?> et);

	Collection<Axiom<?>> loadSimpleList(SimpleListDescriptor listDescriptor);

	Collection<Axiom<?>> loadReferencedList(ReferencedListDescriptor listDescriptor);
}
