package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassW;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class LazyLoadingProxyFactoryTest {
    private static OWLClassA entityA;
    private static OWLClassB entityB;
    private static OWLClassW entityW;

    @BeforeAll
    static void setUpBeforeClass() {
        entityA = new OWLClassA();
        entityB = new OWLClassB();
        entityW = new OWLClassW();
    }

    @Mock
    private UnitOfWork uow;
    private MetamodelMocks metamodelMocks;
    private LazyLoadingProxyFactory lazyLoadingProxyFactory;

    @BeforeEach
    void setUp() throws Exception {
        this.lazyLoadingProxyFactory = new LazyLoadingProxyFactory(uow);
        this.metamodelMocks = new MetamodelMocks();
        final MetamodelImpl mm = mock(MetamodelImpl.class);
        metamodelMocks.setMocks(mm);
        when(uow.getMetamodel()).thenReturn(mm);
    }

    @Test
    void createLazyLoadingSetProxyForSetField() throws NoSuchFieldException {
        EntityType<OWLClassA> entityType = uow.getMetamodel().entity(OWLClassA.class);
        FieldSpecification<? super OWLClassA, ?> fieldSpec = entityType.getFieldSpecification(OWLClassA.getTypesField().getName());
        Object proxy = lazyLoadingProxyFactory.createProxy(entityA, fieldSpec);

        assertInstanceOf(LazyLoadingSetProxy.class, proxy);
    }

    @Test
    void createLazyLoadingMapProxyForMapField() throws NoSuchFieldException {
        EntityType<OWLClassB> entityType = uow.getMetamodel().entity(OWLClassB.class);
        FieldSpecification<? super OWLClassB, ?> fieldSpec = entityType.getFieldSpecification(OWLClassB.getPropertiesField().getName());
        Object proxy = lazyLoadingProxyFactory.createProxy(entityB, fieldSpec);

        assertInstanceOf(LazyLoadingMapProxy.class, proxy);
    }

    @Test
    void createLazyLoadingSettProxyForSetField() throws NoSuchFieldException {
        EntityType<OWLClassW> entityType = uow.getMetamodel().entity(OWLClassW.class);
        FieldSpecification<? super OWLClassW, ?> fieldSpec = entityType.getFieldSpecification(OWLClassW.getSetStringAttField().getName());
        Object proxy = lazyLoadingProxyFactory.createProxy(entityW, fieldSpec);

        assertInstanceOf(LazyLoadingSetProxy.class, proxy);
    }

    @Test
    void createLazyLoadingListProxyForListField() throws NoSuchFieldException {
        EntityType<OWLClassW> entityType = uow.getMetamodel().entity(OWLClassW.class);
        FieldSpecification<? super OWLClassW, ?> fieldSpec = entityType.getFieldSpecification(OWLClassW.getListStringAttField().getName());
        Object proxy = lazyLoadingProxyFactory.createProxy(entityW, fieldSpec);

        assertInstanceOf(LazyLoadingListProxy.class, proxy);
    }

    @Test
    void createLazyLoadingListProxyForCollectionField() throws NoSuchFieldException {
        EntityType<OWLClassW> entityType = uow.getMetamodel().entity(OWLClassW.class);
        FieldSpecification<? super OWLClassW, ?> fieldSpec = entityType.getFieldSpecification(OWLClassW.getCollectionStringAttField().getName());
        Object proxy = lazyLoadingProxyFactory.createProxy(entityW, fieldSpec);

        assertInstanceOf(LazyLoadingSetProxy.class, proxy);
    }

    @Test
    void createLazyLoadingSettProxyForSetQueryField() throws NoSuchFieldException {
        EntityType<OWLClassW> entityType = uow.getMetamodel().entity(OWLClassW.class);
        FieldSpecification<? super OWLClassW, ?> fieldSpec = entityType.getFieldSpecification(OWLClassW.getSetQueryStringAttField().getName());
        Object proxy = lazyLoadingProxyFactory.createProxy(entityW, fieldSpec);

        assertInstanceOf(LazyLoadingSetProxy.class, proxy);
    }

    @Test
    void createLazyLoadingListProxyForListQueryField() throws NoSuchFieldException {
        EntityType<OWLClassW> entityType = uow.getMetamodel().entity(OWLClassW.class);
        FieldSpecification<? super OWLClassW, ?> fieldSpec = entityType.getFieldSpecification(OWLClassW.getListQueryStringAttField().getName());
        Object proxy = lazyLoadingProxyFactory.createProxy(entityW, fieldSpec);

        assertInstanceOf(LazyLoadingListProxy.class, proxy);
    }
}
