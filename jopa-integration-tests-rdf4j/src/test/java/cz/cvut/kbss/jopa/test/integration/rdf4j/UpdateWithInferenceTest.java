package cz.cvut.kbss.jopa.test.integration.rdf4j;

import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateWithInferenceRunner;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Map;

public class UpdateWithInferenceTest extends UpdateWithInferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateWithInferenceTest.class);

    private static final Map<String, String> INFERENCE_PROPS =
            Collections.singletonMap(Rdf4jOntoDriverProperties.USE_INFERENCE, Boolean.TRUE.toString());

    public UpdateWithInferenceTest() {
        super(LOG, new Rdf4jPersistenceFactory(), new Rdf4jDataAccessor());
    }

    @Test
    @Override
    public void settingInferredAttributeFromNullToNewValueWorks() {
        this.em = getEntityManager("settingInferredAttributeFromNullToNewValueWorks", false, INFERENCE_PROPS);
        super.settingInferredAttributeFromNullToNewValueWorks();
    }

    @Test
    @Override
    public void additiveChangeToAttributeWithInferredValuesWorks() throws Exception {
        this.em = getEntityManager("additiveChangeToAttributeWithInferredValuesWorks", false, INFERENCE_PROPS);
        super.additiveChangeToAttributeWithInferredValuesWorks();
    }

    @Test
    @Override
    public void removalOfAssertedValueOfInferredAttributeWorks() throws Exception {
        this.em = getEntityManager("removalOfAssertedValueOfInferredAttributeWorks", false, INFERENCE_PROPS);
        super.removalOfAssertedValueOfInferredAttributeWorks();
    }

    @Test
    @Override
    public void removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException() throws Exception {
        this.em = getEntityManager("removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException", false, INFERENCE_PROPS);
        super.removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException();
    }
}
