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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.junit.Ignore;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public abstract class DeleteOperationsRunner extends BaseRunner {

    public DeleteOperationsRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testRemoveSimple() {
        logger.config("Test: simple entity removal.");
        this.em = getEntityManager("SimpleRemove", false);
        persist(entityA);

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        em.getTransaction().begin();
        em.remove(a);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    // TODO First we need to resolve referential integrity
    @Ignore
    @Test
    public void testRemoveReference() {
        logger.config("Test: remove entity referenced by another entity.");
        this.em = getEntityManager("RemoveReference", false);
        persist(entityD, entityA);

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        em.getTransaction().begin();
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassD res = em.find(OWLClassD.class, entityD.getUri());
        assertNotNull(res);
        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void testRemoveCascade() {
        logger.config("Test: remove cascade.");
        this.em = getEntityManager("RemoveCascade", false);
        em.getTransaction().begin();
        em.persist(entityG);
        assertTrue(em.contains(entityG));
        assertTrue(em.contains(entityH));
        assertTrue(em.contains(entityA));
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassG g = em.find(OWLClassG.class, entityG.getUri());
        final OWLClassH h = em.find(OWLClassH.class, entityH.getUri());
        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(g);
        assertNotNull(h);
        assertNotNull(a);
        assertTrue(em.contains(g));
        assertTrue(em.contains(h));
        assertTrue(em.contains(a));
        assertNotNull(g);
        em.remove(g);
        assertFalse(em.contains(g));
        assertFalse(em.contains(h));
        assertFalse(em.contains(a));
        em.getTransaction().commit();

        assertNull(em.find(OWLClassG.class, entityG.getUri()));
        assertNull(em.find(OWLClassH.class, entityH.getUri()));
        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRemoveDetached() {
        logger.config("Test: try removing detached entity.");
        this.em = getEntityManager("RemoveDetached", false);
        assertNull(entityE.getUri());
        persist(entityE);
        assertNotNull(entityE.getUri());

        em.getTransaction().begin();
        final OWLClassE e = em.find(OWLClassE.class, entityE.getUri());
        assertNotNull(e);
        assertTrue(em.contains(e));
        em.detach(e);
        assertFalse(em.contains(e));
        em.remove(e);
    }

    @Test
    public void testRemoveFromSimpleList() {
        logger.config("Test: remove entity from simple list.");
        this.em = getEntityManager("RemoveFromSimpleList", false);
        final int size = 5;
        entityC.setSimpleList(Generators.createSimpleList(size));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.getTransaction().commit();

        final int randIndex = TestEnvironmentUtils.randomInt(size);
        final OWLClassA a = em.find(OWLClassA.class, entityC.getSimpleList().get(randIndex).getUri());
        assertNotNull(a);
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        // We have to remove A from the simple list as well because otherwise we would break the chain in instances
        assertTrue(c.getSimpleList().remove(a));
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNull(resA);
        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        boolean found = false;
        for (OWLClassA aa : resC.getSimpleList()) {
            if (aa.getUri().equals(a.getUri())) {
                found = true;
                break;
            }
        }
        assertFalse(found);
    }

    @Test
    public void testRemoveFromReferencedList() {
        logger.config("Test: remove entity from referenced list.");
        this.em = getEntityManager("RemoveFromReferencedList", false);
        final int size = 10;
        entityC.setReferencedList(Generators.createReferencedList(size));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

        final int randIndex = TestEnvironmentUtils.randomInt(size);
        final OWLClassA a = em.find(OWLClassA.class, entityC.getReferencedList().get(randIndex).getUri());
        assertNotNull(a);
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        // We have to remove A from the referenced list as well because otherwise we would break the chain in instances
        assertTrue(c.getReferencedList().remove(a));
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNull(resA);
        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        boolean found = false;
        for (OWLClassA aa : resC.getReferencedList()) {
            if (aa.getUri().equals(a.getUri())) {
                found = true;
                break;
            }
        }
        assertFalse(found);
    }

    @Test
    public void testRemoveListOwner() {
        logger.config("Test: remove owner of simple and referenced list.");
        this.em = getEntityManager("RemoveListOwner", false);
        entityC.setSimpleList(Generators.createSimpleList());
        entityC.setReferencedList(Generators.createReferencedList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        em.remove(c);
        em.getTransaction().commit();

        em.getEntityManagerFactory().getCache().evictAll();
        for (OWLClassA a : entityC.getSimpleList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
        for (OWLClassA a : entityC.getReferencedList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
    }

    @Test
    public void testRemoveNotYetCommitted() {
        logger.config("Test: persist entity, but remove it before committing the transaction.");
        this.em = getEntityManager("RemoveNotYetCommitted", false);
        em.getTransaction().begin();
        em.persist(entityE);
        assertTrue(em.contains(entityE));
        em.remove(entityE);
        assertFalse(em.contains(entityE));
        em.getTransaction().commit();

        final OWLClassE res = em.find(OWLClassE.class, entityE.getUri());
        assertNull(res);
    }

    @Test
    public void testCascadeMergeAndRemove() {
        logger.config("Test: merge and remove the merged instance, cascading to another object.");
        this.em = getEntityManager("CascadeMergeAndRemove", false);
        em.getTransaction().begin();
        em.persist(entityG);
        assertTrue(em.contains(entityA));
        assertTrue(em.contains(entityG));
        assertTrue(em.contains(entityH));
        em.getTransaction().commit();
        em.clear();

        final OWLClassG toDetach = em.find(OWLClassG.class, entityG.getUri());
        assertNotNull(toDetach);
        em.detach(toDetach);
        assertFalse(em.contains(toDetach));
        assertFalse(em.contains(toDetach.getOwlClassH()));
        assertFalse(em.contains(toDetach.getOwlClassH().getOwlClassA()));

        em.getTransaction().begin();
        final OWLClassG toRemove = em.merge(toDetach);
        em.remove(toRemove);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassG.class, entityG.getUri()));
        assertNull(em.find(OWLClassH.class, entityH.getUri()));
        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void removeDeletesAllPropertyAssertionsMappedByEntity() throws Exception {
        logger.config(
                "Test: remove deletes all property assertions mapped by the entity, including the lazily loaded ones.");
        this.em = getEntityManager("RemoveDeletesAllMappedAttributes", false);
        em.getTransaction().begin();
        entityC.setSimpleList(Generators.createSimpleList(5));
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.getTransaction().commit();

        final OWLClassC toRemove = em.find(OWLClassC.class, entityC.getUri());
        em.getTransaction().begin();
        em.remove(toRemove);
        em.getTransaction().commit();

        final List<String> properties = resolveCProperties();
        for (String prop : properties) {
            assertFalse(
                    em.createNativeQuery("ASK WHERE { ?x ?p ?o .}", Boolean.class).setParameter("x", entityC.getUri())
                      .setParameter("p", URI.create(prop)).getSingleResult());
        }
    }

    private List<String> resolveCProperties() throws Exception {
        final List<String> lst = new ArrayList<>();
        for (Field f : OWLClassC.class.getDeclaredFields()) {
            if (f.getAnnotation(Id.class) != null || EntityPropertiesUtils.isFieldTransient(f)) {
                continue;
            }
            if (f.getAnnotation(OWLDataProperty.class) != null) {
                lst.add(f.getAnnotation(OWLDataProperty.class).iri());
            } else if (f.getAnnotation(OWLObjectProperty.class) != null) {
                lst.add(f.getAnnotation(OWLObjectProperty.class).iri());
            } else if (f.getAnnotation(OWLAnnotationProperty.class) != null) {
                lst.add(f.getAnnotation(OWLAnnotationProperty.class).iri());
            }
        }
        return lst;
    }

    @Test
    public void testRemoveUnmappedPropertyValue() throws Exception {
        entityB.setProperties(Generators.createProperties());
        this.em = getEntityManager("RemoveUnmappedPropertyValue", false);
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();

        final String property = entityB.getProperties().keySet().iterator().next();
        final Set<String> values = entityB.getProperties().get(property);
        assertTrue(values.size() > 0);
        final String valueToRemove = values.iterator().next();
        em.getTransaction().begin();
        final OWLClassB toUpdate = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(toUpdate.getProperties());
        assertTrue(toUpdate.getProperties().containsKey(property));
        assertTrue(toUpdate.getProperties().get(property).contains(valueToRemove));
        toUpdate.getProperties().get(property).remove(valueToRemove);
        em.getTransaction().commit();

        final OWLClassB result = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(result.getProperties());
        assertTrue(result.getProperties().containsKey(property));
        assertEquals(values.size() - 1, result.getProperties().get(property).size());
        assertFalse(result.getProperties().get(property).contains(valueToRemove));
    }

    @Test
    public void testRemoveAllValuesOfUnmappedProperty() throws Exception {
        entityB.setProperties(Generators.createProperties());
        this.em = getEntityManager("RemoveAllValuesOfUnmappedProperty", false);
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();

        final String property = entityB.getProperties().keySet().iterator().next();
        em.getTransaction().begin();
        final OWLClassB toUpdate = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(toUpdate.getProperties());
        assertTrue(toUpdate.getProperties().containsKey(property));
        toUpdate.getProperties().remove(property);
        em.getTransaction().commit();

        final OWLClassB result = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(result.getProperties());
        assertFalse(result.getProperties().containsKey(property));
    }
}
