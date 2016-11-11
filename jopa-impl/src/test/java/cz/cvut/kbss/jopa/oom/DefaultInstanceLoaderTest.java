package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class DefaultInstanceLoaderTest {

    private static final URI ENTITY_PK = Generators.createIndividualIdentifier();

    private static OWLClassA entityA;
    private static Descriptor aDescriptor;
    private static AxiomDescriptor axiomDescriptor;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private Connection connectionMock;

    @Mock
    private Metamodel metamodelMock;

    @Mock
    private AxiomDescriptorFactory descriptorFactoryMock;

    @Mock
    private EntityConstructor entityConstructorMock;

    private EntityType<OWLClassA> etAMock;
    private LoadingParameters<OWLClassA> loadingParameters;

    private EntityInstanceLoader instanceLoader;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        entityA = new OWLClassA();
        entityA.setUri(ENTITY_PK);
        entityA.setStringAttribute("SomeStringAttribute");
        aDescriptor = new EntityDescriptor();
        axiomDescriptor = new AxiomDescriptor(NamedResource.create(ENTITY_PK));
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.loadingParameters = new LoadingParameters<>(OWLClassA.class, ENTITY_PK, aDescriptor);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.etAMock = mocks.forOwlClassA().entityType();
        when(descriptorFactoryMock.createForEntityLoading(loadingParameters, mocks.forOwlClassA().entityType()))
                .thenReturn(axiomDescriptor);
        when(
                descriptorFactoryMock.createForFieldLoading(ENTITY_PK, OWLClassA.getTypesField(),
                        aDescriptor, mocks.forOwlClassA().entityType())).thenReturn(axiomDescriptor);
        entityA.setTypes(null);
        this.instanceLoader = new DefaultInstanceLoader(connectionMock, metamodelMock, descriptorFactoryMock,
                entityConstructorMock);
    }

    @Test
    public void testLoadEntity() throws Exception {
        final Collection<Axiom<?>> entityAAxioms = Collections.singletonList(mock(Axiom.class));
        when(connectionMock.find(axiomDescriptor)).thenReturn(entityAAxioms);
        when(entityConstructorMock.reconstructEntity(ENTITY_PK, etAMock, aDescriptor, entityAAxioms))
                .thenReturn(entityA);
        final OWLClassA res = instanceLoader.loadEntity(loadingParameters);

        assertNotNull(res);
        assertSame(entityA, res);
        verify(connectionMock).find(axiomDescriptor);
    }

    @Test
    public void testLoadEntityUnknown() throws Exception {
        when(connectionMock.find(axiomDescriptor)).thenReturn(Collections.emptyList());
        final OWLClassA res = instanceLoader.loadEntity(loadingParameters);
        assertNull(res);
        verify(connectionMock).find(axiomDescriptor);
    }

    @Test
    public void testLoadEntityDriverException() throws Exception {
        thrown.expect(StorageAccessException.class);
        when(connectionMock.find(any(AxiomDescriptor.class))).thenThrow(new OntoDriverException());
        instanceLoader.loadEntity(loadingParameters);
    }
}