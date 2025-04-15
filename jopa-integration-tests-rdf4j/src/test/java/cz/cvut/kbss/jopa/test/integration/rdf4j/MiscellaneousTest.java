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
package cz.cvut.kbss.jopa.test.integration.rdf4j;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MiscellaneousTest {

    private final Rdf4jPersistenceFactory persistenceFactory;

    MiscellaneousTest() {
        this.persistenceFactory = new Rdf4jPersistenceFactory();
    }

    @Test
    void unwrapExtractsRdf4jRepository() {
        try (EntityManager em = persistenceFactory.getEntityManager("UnwrapTest", false, Collections.emptyMap())) {
            final Repository repository = em.unwrap(Repository.class);
            assertNotNull(repository);
            assertTrue(repository.isInitialized());
        }
    }
}
