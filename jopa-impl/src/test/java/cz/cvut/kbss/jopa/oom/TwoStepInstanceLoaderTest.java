package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class TwoStepInstanceLoaderTest extends InstanceLoaderTestBase {

    @Mock
    private Types typesMock;

    private LoadingParameters<OWLClassS> loadingParameters;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        staticSetup();
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.loadingParameters = new LoadingParameters<>(OWLClassS.class, ENTITY_PK, descriptor);
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
    public void loadEntityLoadsTypesAndDeterminesEntityTypeBeforeLoadingInstance() throws Exception {
        final Set<Axiom<URI>> types = Collections.singleton(
                new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false),
                        new Value<>(URI.create(OWLClassR.getClassIri()))));
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenReturn(types);

        instanceLoader.loadEntity(loadingParameters);
        verify(typesMock).getTypes(INDIVIDUAL, null, false);
        verify(descriptorFactoryMock).createForEntityLoading(loadingParameters, metamodelMock.entity(OWLClassR.class));
    }

    @Test
    public void loadEntityLoadsEntityFromStorageWhenEntityTypeIsDetermined() throws Exception {
        final Set<Axiom<URI>> types = Collections.singleton(
                new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false),
                        new Value<>(URI.create(OWLClassR.getClassIri()))));
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenReturn(types);
        final OWLClassR entityR = new OWLClassR();
        final Collection<Axiom<?>> axioms = new HashSet<>(types);
        when(connectionMock.find(axiomDescriptor)).thenReturn(axioms);
        when(entityConstructorMock
                .reconstructEntity(ENTITY_PK, metamodelMock.entity(OWLClassR.class), descriptor, axioms))
                .thenReturn(entityR);

        final OWLClassS result = instanceLoader.loadEntity(loadingParameters);
        assertNotNull(result);
        assertSame(entityR, result);
    }

    @Test
    public void loadEntityReturnsNullWhenNoTypesForIndividualAreFound() throws Exception {
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenReturn(Collections.emptySet());

        assertNull(instanceLoader.loadEntity(loadingParameters));
    }

    @Test
    public void loadEntityReturnsNullWhenNoMatchingEntityTypeIsFound() throws Exception {
        final Set<Axiom<URI>> types = Collections.singleton(
                new AxiomImpl<>(INDIVIDUAL, Assertion.createClassAssertion(false),
                        new Value<>(URI.create(OWLClassA.getClassIri()))));
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenReturn(types);

        assertNull(instanceLoader.loadEntity(loadingParameters));
    }

    @Test
    public void loadEntityThrowsStorageAccessExceptionWhenOntoDriverThrowsException() throws Exception {
        final String msg = "Exception message.";
        when(typesMock.getTypes(INDIVIDUAL, null, false)).thenThrow(new OntoDriverException(msg));
        thrown.expect(StorageAccessException.class);
        thrown.expectMessage(msg);

        instanceLoader.loadEntity(loadingParameters);
    }
}
