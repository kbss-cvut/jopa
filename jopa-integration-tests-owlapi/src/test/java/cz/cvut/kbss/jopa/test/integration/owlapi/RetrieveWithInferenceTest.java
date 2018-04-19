package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.OWLClassW;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Triple;
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collections;

import static cz.cvut.kbss.jopa.test.Vocabulary.RDFS_SUBCLASS_OF;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class RetrieveWithInferenceTest extends RetrieveWithInferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveWithInferenceTest.class);

    public RetrieveWithInferenceTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }

    @Test
    @Override
    public void retrievedEntityWithInferredTypesContainsInferredData() throws Exception {
        this.em = getEntityManager("retrievedEntityWithInferredTypesContainsInferredData", false);
        persist(entityW);

        // We have to add the subclass assertion here, so that it is applied to the same transactional snapshot as is
        // queried next
        persistTestData(Collections.singleton(
                new Triple(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS_SUBCLASS_OF),
                        URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        final OWLClassW result = em.find(OWLClassW.class, entityW.getUri());
        assertNotNull(result);
        assertTrue(result.getTypes().contains(URI.create(Vocabulary.C_OWL_CLASS_A)));
    }
}
