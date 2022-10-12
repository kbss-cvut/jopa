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
package cz.cvut.kbss.jopa.test.integration.rdf4j;

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.integration.rdf4j.model.Concept;
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class RetrieveWithInferenceTest extends RetrieveWithInferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveWithInferenceTest.class);

    public RetrieveWithInferenceTest() {
        super(LOG, new Rdf4jPersistenceFactory(), new Rdf4jDataAccessor());
    }

    @Test
    @Override
    public void retrievedEntityWithInferredTypesContainsInferredData() throws Exception {
        final Map<String, String> inferenceProps =
                Collections.singletonMap(Rdf4jOntoDriverProperties.USE_INFERENCE, Boolean.TRUE.toString());
        this.em = getEntityManager("retrievedEntityWithInferredTypesContainsInferredData", false, inferenceProps);
        super.retrievedEntityWithInferredTypesContainsInferredData();
    }

    @Test
    void retrieveSupportsLoadingInferredStatementsFromDefaultContext() {
        final Map<String, String> inferenceProps = new HashMap<>();
        inferenceProps.put(Rdf4jOntoDriverProperties.USE_INFERENCE, Boolean.TRUE.toString());
        inferenceProps.put(Rdf4jOntoDriverProperties.REPOSITORY_CONFIG, "classpath:rdf4j-memory-spin-rdfs.ttl");
        inferenceProps.put(Rdf4jOntoDriverProperties.INFERENCE_IN_DEFAULT_CONTEXT, Boolean.TRUE.toString());
        this.em =
                getEntityManager("RetrieveSupportsLoadingInferredStatementsFromDefaultContext", false, inferenceProps);
        loadSpinRules();
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

    private void loadSpinRules() {
        transactional(() -> {
            final Repository repo = em.unwrap(Repository.class);
            try (final RepositoryConnection conn = repo.getConnection()) {
                final InputStream rulesIn = this.getClass().getClassLoader().getResourceAsStream("test-spin-rules.ttl");
                conn.add(rulesIn, RDFFormat.TURTLE);
            } catch (IOException e) {
                fail(e);
            }
        });
    }
}
