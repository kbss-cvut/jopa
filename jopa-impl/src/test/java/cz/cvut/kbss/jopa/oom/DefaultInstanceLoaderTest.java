/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.FindResult;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.util.Collection;
import java.util.Collections;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
                .thenReturn(new FindResult<>(entityA, null));
        final FindResult<? extends OWLClassA> res = instanceLoader.loadEntity(loadingParameters);
        assertNotNull(res);
        assertTrue(res.getInstance().isPresent());
        assertSame(entityA, res.getInstance().get());
        verify(connectionMock).find(axiomDescriptor);
    }

    @Test
    void testLoadEntityUnknown() throws Exception {
        when(connectionMock.find(axiomDescriptor)).thenReturn(Collections.emptyList());
        final FindResult<? extends OWLClassA> res = instanceLoader.loadEntity(loadingParameters);
        assertFalse(res.getInstance().isPresent());
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
                .thenReturn(new FindResult<>(entityA, null));
        final FindResult<? extends OWLClassA> res = instanceLoader.loadEntity(loadingParameters);
        assertTrue(res.getInstance().isPresent());
        verify(cacheMock, never()).contains(etAMock.getJavaType(), IDENTIFIER, descriptor);
    }
}