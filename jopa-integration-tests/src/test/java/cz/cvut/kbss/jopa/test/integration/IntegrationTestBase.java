/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.EntityManagerFactoryImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.integration.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.integration.environment.TestDataSource;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.doReturn;

public class IntegrationTestBase {

    EntityManagerFactory emf;
    EntityManager em;

    @Mock
    Connection connectionMock;

    @BeforeEach
    protected void setUp() throws Exception {
        this.emf = PersistenceFactory.initPersistence(Collections.emptyMap());
        this.em = emf.createEntityManager();

        final TestDataSource testDs = getDataSource();
        testDs.setConnection(connectionMock);
    }

    @AfterEach
    protected void tearDown() {
        em.close();
        emf.close();
    }

    private TestDataSource getDataSource() {
        return ((EntityManagerFactoryImpl) emf).getServerSession().unwrap(TestDataSource.class);
    }

    void initAxiomsForOWLClassA(NamedResource subject, Assertion stringAss, String stringAtt)
            throws OntoDriverException {
        final List<Axiom<?>> axioms = new ArrayList<>();
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(OWLClassA.getClassIri())));
        axioms.add(classAssertion);
        axioms.add(new AxiomImpl<>(subject, stringAss, new Value<>(stringAtt)));
        final AxiomDescriptor desc = new AxiomDescriptor(subject);
        desc.addAssertion(Assertion.createClassAssertion(false));
        desc.addAssertion(stringAss);
        doReturn(axioms).when(connectionMock).find(desc);
        doReturn(true).when(connectionMock).contains(classAssertion, Collections.emptySet());
    }
}
