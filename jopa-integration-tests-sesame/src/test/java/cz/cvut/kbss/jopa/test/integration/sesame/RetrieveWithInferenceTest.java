package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.test.environment.SesameDataAccessor;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Map;

public class RetrieveWithInferenceTest extends RetrieveWithInferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveWithInferenceTest.class);

    public RetrieveWithInferenceTest() {
        super(LOG, new SesamePersistenceFactory(), new SesameDataAccessor());
    }

    @Test
    @Override
    public void retrievedEntityWithInferredTypesContainsInferredData() throws Exception {
        final Map<String, String> inferenceProps =
                Collections.singletonMap(SesameOntoDriverProperties.SESAME_USE_INFERENCE, Boolean.TRUE.toString());
        this.em = getEntityManager("retrievedEntityWithInferredTypesContainsInferredData", false, inferenceProps);
        super.retrievedEntityWithInferredTypesContainsInferredData();
    }
}
