/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.test.environment.VirtuosoDataAccessor;
import cz.cvut.kbss.jopa.test.environment.VirtuosoPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsRunner;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;

@EnabledIfSystemProperty(named = "virtuoso.host", matches = ".+")
@EnabledIfSystemProperty(named = "virtuoso.port", matches = ".+")
public class CreateOperationsTest extends CreateOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOperationsTest.class);

    public CreateOperationsTest() {
        super(LOG, new VirtuosoPersistenceFactory(), new VirtuosoDataAccessor());
    }

    @AfterEach
    public void tearDown() {
        VirtuosoDataAccessor.clearRepository(em);
        super.tearDown();
    }

    /**
     * Caused by Virtuoso not treating simple literals and xsd:string literals as the same (as per RDF1.1, SPARQL 1.1)
     * See <a href="https://github.com/openlink/virtuoso-opensource/issues/728">...</a>
     */
    @Disabled
    @Test
    @Override
    public void testPersistEntityWithASKQueryAttr() {
        super.testPersistEntityWithASKQueryAttr();
    }

    @Test
    @Override
    public void testPersistTypedProperties() throws Exception {
        this.em = getEntityManager("PersistTypedProperties", false);
        entityP.setProperties(Generators.createTypedProperties());
        // Because of https://github.com/openlink/virtuoso-opensource/issues/1347
        for (Set<Object> values : entityP.getProperties().values()) {
            if (values.contains(1) || values.contains(BigInteger.valueOf(1L))) {
                values.remove(true);
            } else if (values.contains(0) || values.contains(BigInteger.valueOf(0L))) {
                values.remove(false);
            }
        }
        persist(entityP);
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getProperties().keySet(), res.getProperties().keySet());
        for (URI property : entityP.getProperties().keySet()) {
            final Set<Object> expected = entityP.getProperties().get(property);
            final Set<Object> actual = res.getProperties().get(property);
            assertEquals(expected, actual);
        }
        final List<Quad> expectedStatements = new ArrayList<>();
        entityP.getProperties()
               .forEach((k, vs) -> vs.forEach(v -> expectedStatements.add(new Quad(entityP.getUri(), k, v, (String) null))));
        verifyStatementsPresent(expectedStatements, em);
    }
}
