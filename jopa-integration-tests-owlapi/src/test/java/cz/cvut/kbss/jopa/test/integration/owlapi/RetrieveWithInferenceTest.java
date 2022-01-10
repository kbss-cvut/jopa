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
package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.OWLClassW;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                        URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        final OWLClassW result = em.find(OWLClassW.class, entityW.getUri());
        assertNotNull(result);
        assertTrue(result.getTypes().contains(URI.create(Vocabulary.C_OWL_CLASS_A)));
    }
}
