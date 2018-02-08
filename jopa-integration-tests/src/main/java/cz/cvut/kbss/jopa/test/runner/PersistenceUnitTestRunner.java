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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public abstract class PersistenceUnitTestRunner extends BaseRunner {

    public PersistenceUnitTestRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void multiplePersistenceUnitsSaveDataIndependently() {
        final List<EntityManager> ems = initPersistenceUnits();
        try {
            for (EntityManager em : ems) {
                em.getTransaction().begin();
                em.persist(entityA);
                em.getTransaction().commit();
                em.clear();
            }

            for (EntityManager em : ems) {
                assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
                // Cannot use count query, because OWL2Query does not support it
                final List<?> res = em.createNativeQuery("SELECT ?x WHERE {?x a ?type .}")
                                      .setParameter("type", URI.create(OWLClassA.getClassIri())).getResultList();
                assertEquals(1, res.size());
            }
        } finally {
            ems.forEach(em -> {
                em.close();
                em.getEntityManagerFactory().close();
            });
        }
    }

    private List<EntityManager> initPersistenceUnits() {
        final List<EntityManager> ems = new ArrayList<>();
        for (int i = 0; i < Generators.randomPositiveInt(2, 5); i++) {
            ems.add(getEntityManager("MultiplePUsTest" + i, false));
        }
        return ems;
    }
}
