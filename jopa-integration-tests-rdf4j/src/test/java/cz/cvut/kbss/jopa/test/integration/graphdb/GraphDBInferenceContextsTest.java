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
package cz.cvut.kbss.jopa.test.integration.graphdb;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProvider;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jDataSource;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;

import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class GraphDBInferenceContextsTest {

    private static final String VOCABULARY = "http://onto.fel.cvut.cz/ontologies/slovnik/ml-test";

    private Map<String, String> properties;

    private EntityManagerFactory emf;

    @BeforeEach
    void setUp() {
        this.properties = new HashMap<>();
        properties.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        properties.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.test.integration.graphdb");
        properties.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, Rdf4jDataSource.class.getName());
        properties.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, System.getProperty("jopa.graphdb.url"));
        properties.put(JOPAPersistenceProperties.JPA_PERSISTENCE_PROVIDER, JOPAPersistenceProvider.class.getName());
    }

    @AfterEach
    void tearDown() {
        emf.close();
    }

    /**
     * Enhancement #106
     */
    @Test
    @EnabledIfSystemProperty(named = "jopa.graphdb.url", matches = "http(s)?://")
    void inferredStatementsAreLoadedOneByOneEvenWhenDescriptorWithContextIsSpecified() {
        this.emf = Persistence.createEntityManagerFactory("graphdbTestPU", properties);
        final EntityManager em = emf.createEntityManager();
        final URI vocabularyUri = URI.create(VOCABULARY);
        final Descriptor descriptor = new EntityDescriptor(vocabularyUri);
        final List<Term> result = em.createQuery("SELECT t FROM " + Term.class.getSimpleName() + " t WHERE t.vocabulary = :vocabulary", Term.class)
                .setParameter("vocabulary", vocabularyUri)
                .setDescriptor(descriptor).getResultList();
        assertFalse(result.isEmpty());
        result.forEach(t -> assertEquals(vocabularyUri, t.getVocabulary()));
    }

    /**
     * Enhancement #106
     */
    @Test
    @EnabledIfSystemProperty(named = "jopa.graphdb.url", matches = "http(s)?://")
    void inferredStatementsAreLoadedAllEvenWhenDescriptorWithContextIsSpecified() {
        properties.put(Rdf4jOntoDriverProperties.LOAD_ALL_THRESHOLD, "1");
        this.emf = Persistence.createEntityManagerFactory("graphdbTestPU", properties);
        final EntityManager em = emf.createEntityManager();
        final URI vocabularyUri = URI.create(VOCABULARY);
        final Descriptor descriptor = new EntityDescriptor(vocabularyUri);
        final List<Term> result = em.createQuery("SELECT t FROM " + Term.class.getSimpleName() + " t WHERE t.vocabulary = :vocabulary", Term.class)
                .setParameter("vocabulary", vocabularyUri)
                .setDescriptor(descriptor).getResultList();
        assertFalse(result.isEmpty());
        result.forEach(t -> assertEquals(vocabularyUri, t.getVocabulary()));
    }
}
