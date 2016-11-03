/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.EntityManagerFactoryImpl;
import cz.cvut.kbss.jopa.test.integration.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.integration.environment.TestDataSource;
import cz.cvut.kbss.ontodriver.Connection;
import org.junit.After;
import org.junit.Before;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;

public class IntegrationTestBase {

    EntityManagerFactory emf;
    EntityManager em;

    @Mock
    Connection connectionMock;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.emf = PersistenceFactory.initPersistence(Collections.emptyMap());
        this.em = emf.createEntityManager();

        final TestDataSource testDs = getDataSource();
        testDs.setConnection(connectionMock);
    }

    @After
    public void tearDown() throws Exception {
        em.close();
        emf.close();
    }

    TestDataSource getDataSource() {
        return ((EntityManagerFactoryImpl) emf).getServerSession().unwrap(TestDataSource.class);
    }
}
