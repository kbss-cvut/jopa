package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.ConfigurationHolder;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

import java.net.URI;
import java.util.Collection;

interface EntityMappingHelper extends ConfigurationHolder {

	<T> T getEntityFromCacheOrOntology(Class<T> cls, URI primaryKey, Descriptor descriptor);

	<T> EntityType<T> getEntityType(Class<T> cls);

	URI generateIdentifier(EntityType<?> et);

	<T> T getOriginalInstance(T clone);

	public Collection<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor listDescriptor);

	public Collection<Axiom<NamedResource>> loadReferencedList(ReferencedListDescriptor listDescriptor);
}
