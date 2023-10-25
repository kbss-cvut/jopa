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
        this.em = getEntityManager("removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException", false, INFERENCE_PROPS);
        super.removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException();
    }

    @Test
    @Override
    public void removalOfAssertedValueDoesNotAssertInferredValues() throws Exception {
        this.em = getEntityManager("removalOfAssertedValueDoesNotAssertInferredValues", false, INFERENCE_PROPS);
        super.removalOfAssertedValueDoesNotAssertInferredValues();
    }
}
