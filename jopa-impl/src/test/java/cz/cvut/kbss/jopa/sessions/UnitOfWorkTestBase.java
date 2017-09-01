package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.EntityManagerImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;

import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

abstract class UnitOfWorkTestBase {

    static final URI CONTEXT_URI = URI.create("http://testContext");

    Descriptor descriptor;

    OWLClassA entityA;
    OWLClassB entityB;
    OWLClassD entityD;
    OWLClassL entityL;

    @Mock
    MetamodelImpl metamodelMock;

    @Mock
    CacheManager cacheManagerMock;

    @Mock
    ConnectionWrapper storageMock;

    @Mock
    EntityManagerImpl emMock;

    @Mock
    EntityTransaction transactionMock;

    ServerSessionStub serverSessionStub;

    CloneBuilder cloneBuilder;

    UnitOfWorkImpl uow;

    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.descriptor = new EntityDescriptor(CONTEXT_URI);
        this.serverSessionStub = spy(new ServerSessionStub(storageMock));
        when(serverSessionStub.getMetamodel()).thenReturn(metamodelMock);
        when(serverSessionStub.getLiveObjectCache()).thenReturn(cacheManagerMock);
        when(emMock.getTransaction()).thenReturn(transactionMock);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        uow = new UnitOfWorkImpl(serverSessionStub);
        uow.setEntityManager(emMock);
        final Field connectionField = UnitOfWorkImpl.class.getDeclaredField("storage");
        connectionField.setAccessible(true);
        connectionField.set(uow, storageMock);
        final Field cbField = UnitOfWorkImpl.class.getDeclaredField("cloneBuilder");
        cbField.setAccessible(true);
        this.cloneBuilder = spy((CloneBuilder) cbField.get(uow));
        cbField.set(uow, cloneBuilder);
        initEntities();
    }

    private void initEntities() {
        this.entityA = Generators.generateOwlClassAInstance();
        this.entityB = new OWLClassB(Generators.createIndividualIdentifier());
        this.entityD = new OWLClassD(Generators.createIndividualIdentifier());
        entityD.setOwlClassA(entityA);
        this.entityL = new OWLClassL();
        entityL.setUri(Generators.createIndividualIdentifier());
    }
}
