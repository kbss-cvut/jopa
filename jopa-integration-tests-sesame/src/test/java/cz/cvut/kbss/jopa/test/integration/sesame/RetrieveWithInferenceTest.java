/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.SesameDataAccessor;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.integration.sesame.model.Concept;
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

    // TODO
    @Disabled
    @Test
    void retrieveSupportsLoadingInferredStatementsFromDefaultContext() {
        final Map<String, String> inferenceProps = new HashMap<>();
        inferenceProps.put(SesameOntoDriverProperties.SESAME_USE_INFERENCE, Boolean.TRUE.toString());
        inferenceProps.put(SesameOntoDriverProperties.SESAME_REPOSITORY_CONFIG, "classpath:rdf4j-memory-spin-rdfs.ttl");
        inferenceProps.put(SesameOntoDriverProperties.SESAME_INFERENCE_IN_DEFAULT_CONTEXT, Boolean.TRUE.toString());
        this.em =
                getEntityManager("RetrieveSupportsLoadingInferredStatementsFromDefaultContext", false, inferenceProps);
        final URI context = Generators.generateUri();
        final EntityDescriptor descriptor = new EntityDescriptor(context);

        final Concept parent = new Concept();
        final Concept child = new Concept();
        child.setBroader(Collections.singleton(parent));
        transactional(() -> {
            em.persist(parent, descriptor);
            em.persist(child, descriptor);
        });

        final Concept resultParent = em.find(Concept.class, parent.getUri(), descriptor);
        assertNotNull(resultParent);
        assertNotNull(resultParent.getNarrower());
        assertTrue(resultParent.getNarrower().stream().anyMatch(c -> c.getUri().equals(child.getUri())));
    }
}
