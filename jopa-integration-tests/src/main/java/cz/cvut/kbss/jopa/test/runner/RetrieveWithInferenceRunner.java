package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassW;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Triple;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public abstract class RetrieveWithInferenceRunner extends BaseRunner {

    protected OWLClassW entityW = new OWLClassW();

    public RetrieveWithInferenceRunner(Logger logger, PersistenceFactory persistenceFactory,
                                       DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void retrievedEntityWithInferredTypesContainsInferredData() throws Exception {
        persistTestData(Collections.singleton(
                new Triple(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                        URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        final OWLClassW result = em.find(OWLClassW.class, entityW.getUri());
        assertNotNull(result);
        assertTrue(result.getTypes().contains(URI.create(Vocabulary.C_OWL_CLASS_A)));
    }
}
