package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import org.junit.Test;
import org.openrdf.repository.Repository;

import java.util.Collections;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class MiscellaneousTest {

    private SesamePersistenceFactory persistenceFactory;

    public MiscellaneousTest() {
        this.persistenceFactory = new SesamePersistenceFactory();
    }

    @Test
    public void unwrapExtractsSesameRepository() {
        final EntityManager em = persistenceFactory.getEntityManager("UnwrapTest", false, Collections.emptyMap());
        try {
            final Repository repository = em.unwrap(Repository.class);
            assertNotNull(repository);
            assertTrue(repository.isInitialized());
        } finally {
            em.close();
        }
    }
}
