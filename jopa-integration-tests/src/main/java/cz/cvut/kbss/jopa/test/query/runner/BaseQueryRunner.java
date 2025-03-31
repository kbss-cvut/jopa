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
package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Quad;
import org.slf4j.Logger;

import java.util.Collection;

public abstract class BaseQueryRunner {

    protected final Logger logger;
    protected final DataAccessor dataAccessor;

    protected BaseQueryRunner(Logger logger, DataAccessor dataAccessor) {
        this.logger = logger;
        this.dataAccessor = dataAccessor;
    }

    protected void persistTestData(Collection<Quad> data, EntityManager em) throws Exception {
        dataAccessor.persistTestData(data, em);
    }

    protected abstract EntityManager getEntityManager();
}
