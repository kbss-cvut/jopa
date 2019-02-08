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
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.sessions.FindResult;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class TwoStepInstanceLoaderTest extends InstanceLoaderTestBase {

    @Mock
    private Types typesMock;

    private LoadingParameters<OWLClassS> loadingParameters;

    @BeforeAll
    static void setUpBeforeClass() {
        staticSetup();
    }

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.loadingParameters = new LoadingParameters<>(OWLClassS.class, IDENTIFIER, descriptor);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        when(connectionMock.types()).thenReturn(typesMock);
        when(descriptorFactoryMock.createForEntityLoading(loadingParameters, mocks.forOwlClassR().entityType()))
                .thenReturn(axiomDescriptor);
        this.instanceLoader = TwoStepInstanceLoader.builder().connection(connectionMock).metamodel(metamodelMock)
                                                   .cache(cacheMock).descriptorFactory(descriptorFactoryMock)
                                                   .entityBuilder(entityConstructorMock).build();
    }

    @Test
    void loadEntityLoadsTypesAndDeterminesEntityTypeBeforeLoadingInstance() throws Exception {
        final Set<Axiom<URI>> types = Collections.singleton(
                new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false),
                        new Value<>(URI.create(OWLClassR.getClassIri()))));
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenReturn(types);

        instanceLoader.loadEntity(loadingParameters);
        verify(typesMock).getTypes(INDIVIDUAL, null, false);
        verify(descriptorFactoryMock).createForEntityLoading(loadingParameters, metamodelMock.entity(OWLClassR.class));
    }

    @Test
    void loadEntityLoadsEntityFromStorageWhenEntityTypeIsDetermined() throws Exception {
        final Set<Axiom<URI>> types = Collections.singleton(
                new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false),
                        new Value<>(URI.create(OWLClassR.getClassIri()))));
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenReturn(types);
        final OWLClassR entityR = new OWLClassR();
        final Collection<Axiom<?>> axioms = new HashSet<>(types);
        when(connectionMock.find(axiomDescriptor)).thenReturn(axioms);
        when(entityConstructorMock
                .reconstructEntity(IDENTIFIER, metamodelMock.entity(OWLClassR.class), descriptor, axioms))
                .thenReturn(new FindResult<>(entityR, null));

        final FindResult<? extends OWLClassS> result = instanceLoader.loadEntity(loadingParameters);
        assertNotNull(result);
        assertTrue(result.getInstance().isPresent());
        assertSame(entityR, result.getInstance().get());
    }

    @Test
    void loadEntityReturnsNullWhenNoTypesForIndividualAreFound() throws Exception {
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenReturn(Collections.emptySet());

        assertFalse(instanceLoader.loadEntity(loadingParameters).getInstance().isPresent());
    }

    @Test
    void loadEntityReturnsNullWhenNoMatchingEntityTypeIsFound() throws Exception {
        final Set<Axiom<URI>> types = Collections.singleton(
                new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false),
                        new Value<>(URI.create(OWLClassA.getClassIri()))));
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenReturn(types);

        assertFalse(instanceLoader.loadEntity(loadingParameters).getInstance().isPresent());
    }

    @Test
    void loadEntityThrowsStorageAccessExceptionWhenOntoDriverThrowsException() throws Exception {
        final String msg = "Exception message.";
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenThrow(new OntoDriverException(msg));

        final StorageAccessException ex = assertThrows(StorageAccessException.class,
                () -> instanceLoader.loadEntity(loadingParameters));
        assertThat(ex.getMessage(), containsString(msg));
    }
}
