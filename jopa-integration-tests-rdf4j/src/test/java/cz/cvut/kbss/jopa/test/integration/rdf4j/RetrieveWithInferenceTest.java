/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.test.integration.rdf4j;

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.integration.rdf4j.model.Concept;
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import org.eclipse.rdf4j.model.vocabulary.RDF4J;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.junit.jupiter.api.Disabled;
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
    @Override
    public void isInferredReturnsTrueForInferredPropertyValue() throws Exception {
        final Map<String, String> inferenceProps =
                Collections.singletonMap(Rdf4jOntoDriverProperties.USE_INFERENCE, Boolean.TRUE.toString());
        this.em = getEntityManager("isInferredReturnsTrueForInferredPropertyValue", false, inferenceProps);
        super.isInferredReturnsTrueForInferredPropertyValue();
    }

    // Apparently, RDF4J supports only SHACL validation, not SHACL inference with SPARQL rules
    // See https://github.com/eclipse/rdf4j/issues/191
    @Disabled
    @Test
    void retrieveSupportsLoadingInferredStatementsFromDefaultContext() {
        final Map<String, String> inferenceProps = new HashMap<>();
        inferenceProps.put(Rdf4jOntoDriverProperties.USE_INFERENCE, Boolean.TRUE.toString());
        inferenceProps.put(Rdf4jOntoDriverProperties.REPOSITORY_CONFIG, "classpath:rdf4j-memory-shacl-rdfs.ttl");
        inferenceProps.put(Rdf4jOntoDriverProperties.INFERENCE_IN_DEFAULT_CONTEXT, Boolean.TRUE.toString());
        this.em =
                getEntityManager("RetrieveSupportsLoadingInferredStatementsFromDefaultContext", false, inferenceProps);
        loadShaclRules();
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

    private void loadShaclRules() {
        transactional(() -> {
            final Repository repo = em.unwrap(Repository.class);
            try (final RepositoryConnection conn = repo.getConnection()) {
                final InputStream rulesIn = this.getClass().getClassLoader().getResourceAsStream("test-shacl-rules.ttl");
                conn.add(rulesIn, "", RDFFormat.TURTLE, RDF4J.SHACL_SHAPE_GRAPH);
            } catch (IOException e) {
                fail(e);
            }
        });
    }

    @Test
    @Override
    public void findReturnsOnlyAssertedDataWhenDescriptorDisablesInference() throws Exception {
        final Map<String, String> inferenceProps =
                Collections.singletonMap(Rdf4jOntoDriverProperties.USE_INFERENCE, Boolean.TRUE.toString());
        this.em = getEntityManager("findReturnsOnlyAssertedDataWhenDescriptorDisablesInference", false, inferenceProps);
        super.findReturnsOnlyAssertedDataWhenDescriptorDisablesInference();
    }

    @Test
    @Override
    public void selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell() throws Exception {
        final Map<String, String> inferenceProps =
                Collections.singletonMap(Rdf4jOntoDriverProperties.USE_INFERENCE, Boolean.TRUE.toString());
        this.em = getEntityManager("selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell", false, inferenceProps);
        super.selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell();
    }
}
