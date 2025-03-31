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

import cz.cvut.kbss.jopa.test.environment.VirtuosoDataAccessor;
import cz.cvut.kbss.jopa.test.environment.VirtuosoPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsMultiContextRunner;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@EnabledIfSystemProperty(named = "virtuoso.host", matches = ".+")
@EnabledIfSystemProperty(named = "virtuoso.port", matches = ".+")
public class UpdateOperationsMultiContextTest extends UpdateOperationsMultiContextRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsMultiContextTest.class);

    public UpdateOperationsMultiContextTest() {
        super(LOG, new VirtuosoPersistenceFactory(), new VirtuosoDataAccessor());
    }

    @AfterEach
    public void tearDown() {
        VirtuosoDataAccessor.clearRepository(em);
        super.tearDown();
    }
}
