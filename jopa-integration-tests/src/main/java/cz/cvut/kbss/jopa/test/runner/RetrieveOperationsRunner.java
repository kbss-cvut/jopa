/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import org.junit.Test;
import org.slf4j.Logger;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;

public abstract class RetrieveOperationsRunner extends BaseRunner {

    public RetrieveOperationsRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testRetrieveSimple() {
        logger.debug("Test: retrieve a simple entity.");
        this.em = getEntityManager("RetrieveSimple", false);
        persist(entityA);

        em.getEntityManagerFactory().getCache().evictAll();
        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
        assertTrue(entityA.getTypes().containsAll(res.getTypes()));
        assertTrue(em.contains(res));
    }

    @Test(expected = NullPointerException.class)
    public void testRetrieveNull() {
        logger.debug("Test: retrieve null.");
        this.em = getEntityManager("RetrieveNull", false);
        em.find(OWLClassA.class, null);
    }

    @Test
    public void testRetrieveLazy() throws Exception {
        logger.debug("Test: retrieve entity with lazy loaded attribute.");
        this.em = getEntityManager("RetrieveLazy", false);
        persist(entityI);

        final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri());
        assertNotNull(resI);
        final Field f = OWLClassI.class.getDeclaredField("owlClassA");
        f.setAccessible(true);
        Object value = f.get(resI);
        assertNull(value);
        assertNotNull(resI.getOwlClassA());
        value = f.get(resI);
        assertNotNull(value);
        assertEquals(entityA.getUri(), resI.getOwlClassA().getUri());
        assertTrue(em.contains(resI.getOwlClassA()));
    }

    @Test
    public void testRetrieveGenerated() throws Exception {
        logger.debug("Test: persist and retrieve several entities with generated identifiers.");
        this.em = getEntityManager("RetrieveGenerated", false);
        em.getTransaction().begin();
        final int size = 10;
        final List<OWLClassE> lst = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            final OWLClassE e = new OWLClassE();
            e.setStringAttribute("blablabla" + i);
            assertNull(e.getUri());
            em.persist(e);
            assertNotNull(e.getUri());
            lst.add(e);
        }
        em.getTransaction().commit();

        em.clear();
        for (OWLClassE e : lst) {
            final OWLClassE res = em.find(OWLClassE.class, e.getUri());
            assertNotNull(res);
            assertEquals(e.getStringAttribute(), res.getStringAttribute());
        }
    }

    @Test
    public void testRetrieveNotExisting() {
        logger.debug("Test: retrieve entity which does not exist in the specified context.");
        this.em = getEntityManager("RetrieveNotExisting", false);
        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri());
        assertNull(res);
    }

    @Test
    public void testRefresh() {
        logger.debug("Test: refresh entity.");
        this.em = getEntityManager("Refresh", false);
        persist(entityD, entityA);

        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        newA.setStringAttribute("newA");
        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertEquals(d.getOwlClassA(), a);
        d.setOwlClassA(newA);
        em.refresh(d);
        assertEquals(a.getUri(), d.getOwlClassA().getUri());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRefreshNotManaged() {
        logger.debug("Test: refresh entity which is not managed.");
        this.em = getEntityManager("RefreshNotManaged", false);
        persist(entityA);

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        newA.setStringAttribute("newA");
        em.refresh(newA);
    }

    @Test
    public void testRetrieveDifferentType() {
        logger.debug("Test: persist entity but try to retrieve it as a different type.");
        this.em = getEntityManager("RetrieveDifferentType", false);
        persist(entityA);

        final OWLClassB res = em.find(OWLClassB.class, entityA.getUri());
        assertNull(res);
    }

    @Test
    public void testRefreshInstanceWithUnmappedProperties() {
        logger.debug("Test: refresh entity with @Properties field.");
        this.em = getEntityManager("RefreshEntityWithProperties", false);
        final Map<URI, Set<Object>> properties = Generators.createTypedProperties();
        entityP.setProperties(properties);
        persist(entityP);

        final OWLClassP p = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(p);
        p.getProperties().put(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#addedProperty"),
                Collections.singleton("Test"));
        assertNotEquals(properties, p.getProperties());
        em.refresh(p);
        assertEquals(properties, p.getProperties());
    }
}
