/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
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
