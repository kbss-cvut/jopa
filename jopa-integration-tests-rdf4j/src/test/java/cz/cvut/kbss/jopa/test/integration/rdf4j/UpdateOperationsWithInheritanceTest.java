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

import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsWithInheritanceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateOperationsWithInheritanceTest extends UpdateOperationsWithInheritanceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsWithInheritanceTest.class);

    public UpdateOperationsWithInheritanceTest() {
        super(LOG, new Rdf4jPersistenceFactory(), new Rdf4jDataAccessor());
    }
}
