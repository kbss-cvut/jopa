package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.MockitoAnnotations;

import java.util.Collection;
import java.util.Collections;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class DefaultInstanceLoaderTest extends InstanceLoaderTestBase {

    private static OWLClassA entityA;

    private EntityType<OWLClassA> etAMock;
    private LoadingParameters<OWLClassA> loadingParameters;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        entityA = new OWLClassA();
        entityA.setUri(ENTITY_PK);
        entityA.setStringAttribute("SomeStringAttribute");
        staticSetup();
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.loadingParameters = new LoadingParameters<>(OWLClassA.class, ENTITY_PK, descriptor);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.etAMock = mocks.forOwlClassA().entityType();
        when(descriptorFactoryMock.createForEntityLoading(loadingParameters, mocks.forOwlClassA().entityType()))
                .thenReturn(axiomDescriptor);
        when(
                descriptorFactoryMock.createForFieldLoading(ENTITY_PK, OWLClassA.getTypesField(),
                        descriptor, mocks.forOwlClassA().entityType())).thenReturn(axiomDescriptor);
        entityA.setTypes(null);
        this.instanceLoader = DefaultInstanceLoader.builder().connection(connectionMock).metamodel(metamodelMock)
                                                   .descriptorFactory(descriptorFactoryMock).cache(cacheMock)
                                                   .entityBuilder(entityConstructorMock).build();
    }

    @Test
    public void testLoadEntity() throws Exception {
        final Collection<Axiom<?>> entityAAxioms = Collections.singletonList(mock(Axiom.class));
        when(connectionMock.find(axiomDescriptor)).thenReturn(entityAAxioms);
        when(entityConstructorMock.reconstructEntity(ENTITY_PK, etAMock, descriptor, entityAAxioms))
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