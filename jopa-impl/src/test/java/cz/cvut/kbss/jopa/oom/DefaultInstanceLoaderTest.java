/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.util.Collection;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class DefaultInstanceLoaderTest extends InstanceLoaderTestBase {

    private static OWLClassA entityA;

    private EntityType<OWLClassA> etAMock;
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
        MockitoAnnotations.initMocks(this);
        this.loadingParameters = new LoadingParameters<>(OWLClassA.class, IDENTIFIER, descriptor);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.etAMock = mocks.forOwlClassA().entityType();
        when(descriptorFactoryMock.createForEntityLoading(loadingParameters, mocks.forOwlClassA().entityType()))
                .thenReturn(axiomDescriptor);
        when(
                descriptorFactoryMock.createForFieldLoading(IDENTIFIER, OWLClassA.getTypesField(),
                        descriptor, mocks.forOwlClassA().entityType())).thenReturn(axiomDescriptor);
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
        when(connectionMock.contains(typeAxiom, null)).thenReturn(true);
        when(descriptorFactoryMock.createForReferenceLoading(IDENTIFIER, etAMock)).thenReturn(typeAxiom);
        when(entityConstructorMock.createEntityInstance(IDENTIFIER, etAMock)).thenReturn(entityA);

        final OWLClassA result = instanceLoader.loadReference(loadingParameters);
        assertNotNull(result);
        verify(descriptorFactoryMock).createForReferenceLoading(IDENTIFIER, etAMock);
        verify(connectionMock).contains(typeAxiom, null);
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
        verify(connectionMock).contains(typeAxiom, descriptor.getContext());
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
}