package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateWithInferenceRunner;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Map;

public class UpdateWithInferenceTest extends UpdateWithInferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateWithInferenceTest.class);

    private static final Map<String, String> INFERENCE_PROPS =
            Collections.singletonMap(OntoDriverProperties.REASONER_FACTORY_CLASS,
                                     RDFSRuleReasonerFactory.class.getName());

    public UpdateWithInferenceTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }

    @Test
    @Override
    public void settingInferredAttributeFromNullToNewValueWorks() {
        this.em = getEntityManager("settingInferredAttributeFromNullToNewValueWorks", false, INFERENCE_PROPS);
        super.settingInferredAttributeFromNullToNewValueWorks();
    }

    @Test
    @Override
    public void settingSingularInferredAttributeFromOneAssertedValueToAnotherWorks() {
        this.em = getEntityManager("settingSingularInferredAttributeFromOneAssertedValueToAnotherWorks", false, INFERENCE_PROPS);
        super.settingSingularInferredAttributeFromOneAssertedValueToAnotherWorks();
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
        this.em = getEntityManager("removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException",
                                   false, INFERENCE_PROPS);
        super.removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException();
    }

    @Test
    @Override
    public void removalOfAssertedValueDoesNotAssertInferredValues() throws Exception {
        this.em = getEntityManager("removalOfAssertedValueDoesNotAssertInferredValues", true, INFERENCE_PROPS);
        super.removalOfAssertedValueDoesNotAssertInferredValues();
    }
}
