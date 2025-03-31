/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.integration.virtuoso;

import cz.cvut.kbss.jopa.test.OWLClassP;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.VirtuosoDataAccessor;
import cz.cvut.kbss.jopa.test.environment.VirtuosoPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RetrieveOperationsRunner;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

@EnabledIfSystemProperty(named = "virtuoso.host", matches = ".+")
@EnabledIfSystemProperty(named = "virtuoso.port", matches = ".+")
public class RetrieveOperationsTest extends RetrieveOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveOperationsTest.class);

    public RetrieveOperationsTest() {
        super(LOG, new VirtuosoPersistenceFactory(), new VirtuosoDataAccessor());
    }

    @AfterEach
    public void tearDown() {
        VirtuosoDataAccessor.clearRepository(em);
        super.tearDown();
    }

    @Disabled
    @Test
    @Override
    public void reloadAllowsToReloadFileStorageContent() {
        // Do nothing, Virtuoso driver does not support accessing plain RDF files
    }

    @Test
    @Override
    public void testRefreshInstanceWithUnmappedProperties() {
        this.em = getEntityManager("RefreshEntityWithProperties", false);
        final Map<URI, Set<Object>> properties = Generators.createTypedProperties();
        entityP.setProperties(properties);
        // Because of https://github.com/openlink/virtuoso-opensource/issues/1347
        for (Set<Object> values : entityP.getProperties().values()) {
            if (values.contains(1) || values.contains(BigInteger.valueOf(1L))) {
                values.remove(true);
            } else if (values.contains(0) || values.contains(BigInteger.valueOf(0L))) {
                values.remove(false);
            }
        }
        persist(entityP);

        final OWLClassP p = findRequired(OWLClassP.class, entityP.getUri());
        p.getProperties().put(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#addedProperty"),
                Collections.singleton("Test"));
        assertNotEquals(properties, p.getProperties());
        em.refresh(p);
        assertEquals(properties, p.getProperties());
    }

    @Override
    protected void addFileStorageProperties(Map<String, String> properties) {
        properties.put(Rdf4jOntoDriverProperties.USE_VOLATILE_STORAGE, Boolean.toString(false));
    }
}
