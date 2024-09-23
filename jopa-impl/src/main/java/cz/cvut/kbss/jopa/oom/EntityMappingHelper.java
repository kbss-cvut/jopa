/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.ConfigurationHolder;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;

interface EntityMappingHelper extends ConfigurationHolder {

    <T> T getEntityFromCacheOrOntology(Class<T> cls, URI identifier, Descriptor descriptor);

    <T> EntityType<T> getEntityType(Class<T> cls);

    boolean isManagedType(Class<?> cls);

    URI generateIdentifier(EntityType<?> et);

    <T> T getOriginalInstance(T clone);

    Collection<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor listDescriptor);

    Collection<Axiom<?>> loadReferencedList(ReferencedListDescriptor listDescriptor);

    Collection<Axiom<?>> loadRdfContainer(ContainerDescriptor containerDescriptor);

    boolean isInferred(Axiom<?> axiom, URI context);
}
