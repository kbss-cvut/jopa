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
package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runner.PersistenceUnitTestRunner;
import cz.cvut.kbss.ontodriver.jena.JenaDataSource;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.jupiter.api.Assertions.*;

class PersistenceUnitTest extends PersistenceUnitTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(PersistenceUnitTest.class);

    PersistenceUnitTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }

    @Test
    void setDatasetReplacesInMemoryDatasetInDriver() throws Exception {
        this.em = getEntityManager("setDatasetReplacesInMemoryDatasetInDriver", false);
        final JenaDataSource ds = em.getEntityManagerFactory().unwrap(JenaDataSource.class);
        assertNull(em.find(OWLClassA.class, entityA.getUri()));

        final Dataset newDataset = DatasetFactory.createTxnMem();
        newDataset.getDefaultModel().add(Arrays
                .asList(createStatement(createResource(entityA.getUri().toString()), RDF.type,
                        createResource(Vocabulary.C_OWL_CLASS_A)),
                        createStatement(createResource(entityA.getUri().toString()),
                                createProperty(Vocabulary.P_A_STRING_ATTRIBUTE),
                                createLangLiteral(entityA.getStringAttribute(), TestEnvironment.PERSISTENCE_LANGUAGE))
                ));
        ds.setDataset(newDataset);
        final EntityManager anotherEm = em.getEntityManagerFactory().createEntityManager();
        try {
            final OWLClassA result = anotherEm.find(OWLClassA.class, entityA.getUri());
            assertNotNull(result);
            assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
        } finally {
            anotherEm.close();
        }
    }
}
