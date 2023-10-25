/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Map;

public class RetrieveWithInferenceTest extends RetrieveWithInferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveWithInferenceTest.class);

    public RetrieveWithInferenceTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }

    @Test
    @Override
    public void retrievedEntityWithInferredTypesContainsInferredData() throws Exception {
        final Map<String, String> inferenceProps = Collections.singletonMap(OntoDriverProperties.REASONER_FACTORY_CLASS,
                                                                            RDFSRuleReasonerFactory.class.getName());
        this.em = getEntityManager("retrievedEntityWithInferredTypesContainsInferredData", false, inferenceProps);
        super.retrievedEntityWithInferredTypesContainsInferredData();
    }

    @Test
    @Override
    public void isInferredReturnsTrueForInferredPropertyValue() throws Exception {
        final Map<String, String> inferenceProps = Collections.singletonMap(OntoDriverProperties.REASONER_FACTORY_CLASS,
                                                                            RDFSRuleReasonerFactory.class.getName());
        this.em = getEntityManager("isInferredReturnsTrueForInferredPropertyValue", false, inferenceProps);
        super.isInferredReturnsTrueForInferredPropertyValue();
    }

    @Test
    @Override
    public void findReturnsOnlyAssertedDataWhenDescriptorDisablesInference() throws Exception {
        final Map<String, String> inferenceProps = Collections.singletonMap(OntoDriverProperties.REASONER_FACTORY_CLASS,
                                                                            RDFSRuleReasonerFactory.class.getName());
        this.em = getEntityManager("findReturnsOnlyAssertedDataWhenDescriptorDisablesInference", false, inferenceProps);
        super.findReturnsOnlyAssertedDataWhenDescriptorDisablesInference();
    }

    @Test
    @Override
    public void selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell() throws Exception {
        final Map<String, String> inferenceProps = Collections.singletonMap(OntoDriverProperties.REASONER_FACTORY_CLASS,
                                                                            RDFSRuleReasonerFactory.class.getName());
        this.em = getEntityManager("selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell", false, inferenceProps);
        super.selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell();
    }
}
