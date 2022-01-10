/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.test.environment.SesameDataAccessor;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import org.junit.jupiter.api.Test;
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
