package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runners.CreateOperationsRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestCreateOperationsMemoryStore {

    private static final Logger LOG = Logger.getLogger(TestCreateOperationsMemoryStore.class
            .getName());

    private static final StorageConfig storage = initStorage();
    private static final Map<String, String> properties = initProperties();

    private CreateOperationsRunner runner;

    private EntityManager em;

    @Before
    public void setUp() {
        this.runner = new CreateOperationsRunner(LOG);
    }

    @After
    public void tearDown() throws Exception {
        if (em.isOpen()) {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            em.close();
            em.getEntityManagerFactory().close();
        }
    }

    @Test
    public void testPersistWithGenerated() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistWithGenerated", storage, false,
                properties);
        runner.persistWithGenerated(em, context());
    }

    @Test(expected = PrimaryKeyNotSetException.class)
    public void testPersistWithoutId() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistWithoutId", storage, false,
                properties);
        runner.persistWithoutId(em, context());
    }

    @Test(expected = NullPointerException.class)
    public void testPersistNull() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistNull", storage, false,
                properties);
        runner.persistNull(em, context());
    }

    @Test
    public void testPersistRollback() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistRollback", storage, false,
                properties);
        runner.persistRollback(em, context());
    }

    @Test(expected = RollbackException.class)
    public void testPersistRollbackOnly() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistRollbackOnly", storage, false,
                properties);
        runner.persistRollbackOnly(em, context());
    }

    @Test
    public void testPersistCascade() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistWithCascade", storage, false,
                properties);
        runner.persistCascade(em, context());
    }

    @Test(expected = OWLEntityExistsException.class)
    public void testPersistTwiceInOne() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistTwice", storage, false,
                properties);
        runner.persistTwice(em, context());
    }

    @Test(expected = RollbackException.class)
    public void testPersistWithoutCascade() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistWithoutCascade", storage, false,
                properties);
        runner.persistWithoutCascade(em, context());
    }

    @Test(expected = OWLEntityExistsException.class)
    public void testPersistDetached() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistDetached", storage, false,
                properties);
        runner.persistDetachedEntity(em, context());
    }

    @Test
    public void testPersistSimpleList() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistSimpleList", storage, false,
                properties);
        runner.persistSimpleList(em, context());
    }

    @Test(expected = RollbackException.class)
    public void testPersistSimpleListNoCascade() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistSimpleListNoCascade", storage,
                false, properties);
        runner.persistSimpleListNoCascade(em, context());
    }

    @Test
    public void testPersistReferencedList() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistReferencedList", storage, false,
                properties);
        runner.persistReferencedList(em, context());
    }

    @Test(expected = RollbackException.class)
    public void testPersistReferencedListNoCascade() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistReferencedListNoCascade",
                storage, false, properties);
        runner.persistReferencedListNoCascade(em, context());
    }

    @Test
    public void testPersistSimpleAndReferencedList() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistSimpleAndReferencedList",
                storage, false, properties);
        runner.persistSimpleAndReferencedList(em, context());
    }

    @Test
    public void testPersistProperties() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistWithProperties", storage, false,
                properties);
        runner.persistProperties(em, context());
    }

    @Test
    public void testPersistPropertiesEmpty() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistWithPropertiesEmpty", storage,
                false, properties);
        runner.persistPropertiesEmpty(em, context());
    }

    @Test(expected = OWLEntityExistsException.class)
    public void persistURITwiceInDifferentClasses() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistURITwiceInDifferentClasses",
                storage, false, properties);
        runner.persistURITwiceInDifferentClasses(em, context());
    }

    @Test
    public void testPersistEntityWithBasicTypeAttributes() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistEntityWithBasicTypeAttributes", storage, false,
                properties);
        runner.persistEntityWithBasicTypedDataProperties(em, context());
    }

    @Test
    public void testPersistAndUpdateAttributeBeforeCommit() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistAndUpdateBeforeCommit", storage, false,
                properties);
        runner.setAttributeValueAfterPersistButBeforeCommit(em, context());
    }

    @Test
    public void testPersistEntityWithEnumAttribute() {
        em = TestEnvironment.getPersistenceConnector("SesamePersistEntityWithEnum", storage, false,
                properties);
        runner.persistEntityWithEnum(em, context());
    }

    private URI context() {
        // Don't use contexts for now
        return null;
    }

    private static StorageConfig initStorage() {
        return new SesameMemoryStorageConfig();
    }

    private static Map<String, String> initProperties() {
        final Map<String, String> map = new HashMap<>();
        map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        map.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
        map.put(OWLAPIPersistenceProperties.LANG, "en");
        map.put("storage", "new");
        return map;
    }
}
