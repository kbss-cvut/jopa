/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Collection;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyCollection;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DefaultInstanceLoaderTest extends InstanceLoaderTestBase {

    private static OWLClassA entityA;

    private MetamodelMocks metamodelMocks;
    private IdentifiableEntityType<OWLClassA> etAMock;
    private LoadingParameters<OWLClassA> loadingParameters;

    @BeforeAll
    static void setUpBeforeClass() {
        entityA = new OWLClassA();
        entityA.setUri(IDENTIFIER);
        entityA.setStringAttribute("SomeStringAttribute");
        staticSetup();
    }

    @BeforeEach
    void setUp() throws Exception {
        this.loadingParameters = new LoadingParameters<>(OWLClassA.class, IDENTIFIER, descriptor);
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodelMock);
        this.etAMock = metamodelMocks.forOwlClassA().entityType();
        when(descriptorFactoryMock.createForEntityLoading(loadingParameters, metamodelMocks.forOwlClassA()
                .entityType()))
                .thenReturn(axiomDescriptor);
        when(
                descriptorFactoryMock.createForFieldLoading(IDENTIFIER, metamodelMocks.forOwlClassA().typesSpec(),
                        descriptor, metamodelMocks.forOwlClassA().entityType())).thenReturn(axiomDescriptor);
        entityA.setTypes(null);
        this.instanceLoader = DefaultInstanceLoader.builder().connection(connectionMock).metamodel(metamodelMock)
                .descriptorFactory(descriptorFactoryMock).cache(cacheMock)
                .entityBuilder(entityConstructorMock).build();
    }

    @Test
    void testLoadEntity() throws Exception {
        final Collection<Axiom<?>> entityAAxioms = Collections.singletonList(mock(Axiom.class));
        when(connectionMock.find(axiomDescriptor)).thenReturn(entityAAxioms);
        when(entityConstructorMock.reconstructEntity(IDENTIFIER, etAMock, descriptor, entityAAxioms))
                .thenReturn(entityA);
        final OWLClassA res = instanceLoader.loadEntity(loadingParameters);

        assertNotNull(res);
        assertSame(entityA, res);
        verify(connectionMock).find(axiomDescriptor);
    }

    @Test
    void testLoadEntityUnknown() throws Exception {
        when(connectionMock.find(axiomDescriptor)).thenReturn(Collections.emptyList());
        final OWLClassA res = instanceLoader.loadEntity(loadingParameters);
        assertNull(res);
        verify(connectionMock).find(axiomDescriptor);
    }

    @Test
    void testLoadEntityDriverException() throws Exception {
        when(connectionMock.find(any(AxiomDescriptor.class))).thenThrow(new OntoDriverException());
        assertThrows(StorageAccessException.class, () -> instanceLoader.loadEntity(loadingParameters));
    }

    @Test
    void loadEntityBypassesCacheWhenConfiguredTo() throws Exception {
        loadingParameters.bypassCache();
        final Collection<Axiom<?>> entityAAxioms = Collections.singletonList(mock(Axiom.class));
        when(connectionMock.find(axiomDescriptor)).thenReturn(entityAAxioms);
        when(entityConstructorMock.reconstructEntity(IDENTIFIER, etAMock, descriptor, entityAAxioms))
                .thenReturn(entityA);
        final OWLClassA res = instanceLoader.loadEntity(loadingParameters);
        assertNotNull(res);
        verify(cacheMock, never()).contains(etAMock.getJavaType(), IDENTIFIER, descriptor);
    }

    @Test
    void loadReferenceVerifiesClassAssertionExistenceAndBuildsEntityInstanceWithIdentifier() throws Exception {
        final Axiom<NamedResource> typeAxiom =
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(
                                Vocabulary.c_OwlClassA)));
        when(connectionMock.contains(typeAxiom, Collections.emptySet())).thenReturn(true);
        when(descriptorFactoryMock.createForReferenceLoading(IDENTIFIER, etAMock)).thenReturn(typeAxiom);
        when(entityConstructorMock.createEntityInstance(IDENTIFIER, etAMock)).thenReturn(entityA);

        final OWLClassA result = instanceLoader.loadReference(loadingParameters);
        assertNotNull(result);
        verify(descriptorFactoryMock).createForReferenceLoading(IDENTIFIER, etAMock);
        verify(connectionMock).contains(typeAxiom, Collections.emptySet());
        verify(entityConstructorMock).createEntityInstance(IDENTIFIER, etAMock);
    }

    @Test
    void loadReferenceChecksForClassAssertionInCorrectContext() throws Exception {
        final EntityDescriptor descriptor = new EntityDescriptor(Generators.createIndividualIdentifier());
        final Axiom<NamedResource> typeAxiom =
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(
                                Vocabulary.c_OwlClassA)));
        when(connectionMock.contains(eq(typeAxiom), any())).thenReturn(true);
        when(descriptorFactoryMock.createForReferenceLoading(IDENTIFIER, etAMock)).thenReturn(typeAxiom);
        when(entityConstructorMock.createEntityInstance(IDENTIFIER, etAMock)).thenReturn(entityA);

        final OWLClassA result =
                instanceLoader.loadReference(new LoadingParameters<>(OWLClassA.class, IDENTIFIER, descriptor));
        assertNotNull(result);
        verify(connectionMock).contains(typeAxiom, descriptor.getContexts());
    }

    @Test
    void loadReferenceReturnsNullWhenStorageDoesNotContainTypeAssertionAxiom() throws Exception {
        final Axiom<NamedResource> typeAxiom =
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(
                                Vocabulary.c_OwlClassA)));
        when(connectionMock.contains(any(), any())).thenReturn(false);
        when(descriptorFactoryMock.createForReferenceLoading(IDENTIFIER, etAMock)).thenReturn(typeAxiom);
        when(entityConstructorMock.createEntityInstance(IDENTIFIER, etAMock)).thenReturn(entityA);

        final OWLClassA result = instanceLoader.loadReference(loadingParameters);
        assertNull(result);
        verify(entityConstructorMock, never()).createEntityInstance(IDENTIFIER, etAMock);
    }

    @Test
    void loadReferenceThrowsStorageAccessExceptionOnDriverException() throws Exception {
        final Axiom<NamedResource> typeAxiom =
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(
                                Vocabulary.c_OwlClassA)));
        when(descriptorFactoryMock.createForReferenceLoading(IDENTIFIER, etAMock)).thenReturn(typeAxiom);
        when(connectionMock.contains(any(), any())).thenThrow(new OntoDriverException());

        assertThrows(StorageAccessException.class, () -> instanceLoader.loadReference(loadingParameters));
        verify(entityConstructorMock, never()).createEntityInstance(IDENTIFIER, etAMock);
    }

    @Test
    void loadEntityReloadsQueryAttributesWhenInstanceIsRetrievedFromCache() {
        when(cacheMock.contains(OWLClassA.class, loadingParameters.getIdentifier(), descriptor)).thenReturn(true);
        when(cacheMock.get(OWLClassA.class, loadingParameters.getIdentifier(), descriptor)).thenReturn(entityA);

        final OWLClassA res = instanceLoader.loadEntity(loadingParameters);
        assertEquals(entityA, res);
        verify(entityConstructorMock).populateQueryAttributes(entityA, etAMock);
        verify(entityConstructorMock, never()).reconstructEntity(eq(loadingParameters.getIdentifier()), eq(etAMock), eq(descriptor), anyCollection());
    }

    @Test
    void loadEntityRecursivelyReloadsQueryAttributesWhenInstanceIsRetrievedFromCache() {
        final OWLClassD entityD = new OWLClassD(Generators.createIndividualIdentifier());
        entityD.setOwlClassA(entityA);
        final LoadingParameters<OWLClassD> dLoadingParameters = new LoadingParameters<>(OWLClassD.class, entityD.getUri(), descriptor);
        when(cacheMock.contains(OWLClassD.class, dLoadingParameters.getIdentifier(), descriptor)).thenReturn(true);
        when(cacheMock.get(OWLClassD.class, dLoadingParameters.getIdentifier(), descriptor)).thenReturn(entityD);

        final OWLClassD result = instanceLoader.loadEntity(dLoadingParameters);
        assertEquals(entityD, result);
        verify(entityConstructorMock).populateQueryAttributes(entityD, metamodelMocks.forOwlClassD().entityType());
        verify(entityConstructorMock).populateQueryAttributes(entityA, etAMock);
    }
}
