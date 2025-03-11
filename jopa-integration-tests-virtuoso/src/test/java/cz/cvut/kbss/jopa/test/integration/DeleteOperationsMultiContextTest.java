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
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.test.environment.VirtuosoDataAccessor;
import cz.cvut.kbss.jopa.test.environment.VirtuosoPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsMultiContextRunner;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@EnabledIfSystemProperty(named = "virtuoso.host", matches = ".+")
@EnabledIfSystemProperty(named = "virtuoso.port", matches = ".+")
public class DeleteOperationsMultiContextTest extends DeleteOperationsMultiContextRunner {

    private static final Logger LOG = LoggerFactory.getLogger(DeleteOperationsMultiContextTest.class);

    public DeleteOperationsMultiContextTest() {
        super(LOG, new VirtuosoPersistenceFactory(), new VirtuosoDataAccessor());
    }

    @AfterEach
    public void tearDown() {
        ((VirtuosoDataAccessor) dataAccessor).clearRepository(em);
        super.tearDown();
    }
}
