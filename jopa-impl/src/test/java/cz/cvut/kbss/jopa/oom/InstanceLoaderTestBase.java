/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import java.net.URI;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

abstract class InstanceLoaderTestBase {

    static final URI IDENTIFIER = Generators.createIndividualIdentifier();
    static final NamedResource INDIVIDUAL = NamedResource.create(IDENTIFIER);

    static Descriptor descriptor;
    static AxiomDescriptor axiomDescriptor;

    @Mock
    Connection connectionMock;

    @Mock
    MetamodelImpl metamodelMock;

    @Mock
    CacheManager cacheMock;

    @Mock
    AxiomDescriptorFactory descriptorFactoryMock;

    @Mock
    EntityConstructor entityConstructorMock;

    EntityInstanceLoader instanceLoader;

    static void staticSetup() {
        descriptor = new EntityDescriptor();
        axiomDescriptor = new AxiomDescriptor(NamedResource.create(IDENTIFIER));
    }

    @Test
    void loadEntityFromAxiomsReturnsCachedInstanceWhenItExists() {
        final Set<Axiom<?>> axioms = Set.of(
                new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false), new Value<>(URI.create(Vocabulary.c_OwlClassA)))
        );
        final OWLClassA cached = new OWLClassA(INDIVIDUAL.getIdentifier());
        when(cacheMock.get(OWLClassA.class, IDENTIFIER, descriptor)).thenReturn(cached);

        final OWLClassA result = instanceLoader.loadEntityFromAxioms(new LoadingParameters<>(OWLClassA.class, INDIVIDUAL.getIdentifier(), descriptor), axioms);
        assertSame(cached, result);
        verify(cacheMock).get(OWLClassA.class, IDENTIFIER, descriptor);
    }
}
