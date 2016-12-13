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
package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.Triple;
import cz.cvut.kbss.jopa.test.environment.OwlapiDataPersist;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsRunner;
import org.junit.Ignore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

public class DeleteOperationsTest extends DeleteOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(DeleteOperationsTest.class);

    private final OwlapiPersistenceFactory persistenceFactory;
    private final OwlapiDataPersist dataPersist;

    public DeleteOperationsTest() throws Exception {
        super(LOG);
        this.persistenceFactory = new OwlapiPersistenceFactory();
        this.dataPersist = new OwlapiDataPersist();
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
        dataPersist.persistTestData(data, em);
    }

    @Ignore
    @Override
    public void settingDatatypeCollectionToNullRemovesAllValues() {
        // Another issue with OWL2Query, causes reasoner exception
    }

    @Ignore
    @Override
    public void clearingDatatypeCollectionRemovesAllValues() {
        // Another issue with OWL2Query, causes reasoner exception
    }
}
