/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesameDataAccessor;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Triple;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsWithInheritanceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

public class UpdateOperationsWithInheritanceTest extends UpdateOperationsWithInheritanceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsWithInheritanceTest.class);

    private final SesamePersistenceFactory persistenceFactory;
    private final SesameDataAccessor dataAccessor;

    public UpdateOperationsWithInheritanceTest() {
        super(LOG);
        this.persistenceFactory = new SesamePersistenceFactory();
        this.dataAccessor = new SesameDataAccessor();
    }

    @Override
    protected EntityManager getEntityManager(String repositoryName, boolean cacheEnabled) {
        return getEntityManager(repositoryName, cacheEnabled, Collections.emptyMap());
    }

    @Override
    protected EntityManager getEntityManager(String repositoryName, boolean cacheEnabled,
                                             Map<String, String> properties) {
        return persistenceFactory.getEntityManager(repositoryName, cacheEnabled, properties);
    }

    @Override
    protected void persistTestData(Collection<Triple> data, EntityManager em) throws Exception {
        dataAccessor.persistTestData(data, em);
    }

    @Override
    protected void verifyStatementsPresent(Collection<Triple> expected, EntityManager em) throws Exception {
        dataAccessor.verifyDataPresence(expected, em);
    }
}
