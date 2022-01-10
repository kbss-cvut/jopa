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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassW;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class RetrieveWithInferenceRunner extends BaseRunner {

    protected OWLClassW entityW = new OWLClassW();

    public RetrieveWithInferenceRunner(Logger logger, PersistenceFactory persistenceFactory,
                                       DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void retrievedEntityWithInferredTypesContainsInferredData() throws Exception {
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                        URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        final OWLClassW result = findRequired(OWLClassW.class, entityW.getUri());
        assertTrue(result.getTypes().contains(URI.create(Vocabulary.C_OWL_CLASS_A)));
    }
}
