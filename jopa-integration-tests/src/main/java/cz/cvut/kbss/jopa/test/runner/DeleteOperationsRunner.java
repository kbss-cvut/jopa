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

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;

public abstract class DeleteOperationsRunner extends BaseRunner {

    public DeleteOperationsRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void testRemoveSimple() {
        this.em = getEntityManager("SimpleRemove", false);
        persist(entityA);

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        em.getTransaction().begin();
        em.remove(a);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassA.class, entityA.getUri()));
        verifyIndividualWasRemoved(entityA.getUri());
    }

    // TODO First we need to resolve referential integrity
    @Ignore
    @Test
    public void testRemoveReference() {
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
        verifyIndividualWasRemoved(entityG.getUri());
        verifyIndividualWasRemoved(entityH.getUri());
        verifyIndividualWasRemoved(entityA.getUri());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRemoveDetached() {
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
        this.em = getEntityManager("RemoveFromSimpleList", false);
        final int size = 5;
        entityC.setSimpleList(Generators.createSimpleList(10));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.getTransaction().commit();

        final int randIndex = Generators.randomInt(size);
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
        this.em = getEntityManager("RemoveFromReferencedList", false);
        final int size = 10;
        entityC.setReferencedList(Generators.createReferencedList(size));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

        final int randIndex = Generators.randomInt(size);
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
    public void testRemoveMergedWithCascading() {
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
    public void removeDeletesAllPropertyAssertionsMappedByEntity() {
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

    private List<String> resolveCProperties() {
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
    public void testRemoveUnmappedPropertyValue() {
        entityB.setProperties(Generators.createProperties());
        this.em = getEntityManager("RemoveUnmappedPropertyValue", false);
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();

        final String property = entityB.getProperties().keySet().iterator().next();
        final Set<String> values = entityB.getProperties().get(property);
        assertFalse(values.isEmpty());
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
    public void testRemoveAllValuesOfUnmappedProperty() {
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

    @Test
    public void testRemoveTypedUnmappedPropertyValue() {
        this.em = getEntityManager("RemoveUnmappedPropertyValueTyped", false);
        entityP.setProperties(Generators.createTypedProperties(10));
        persist(entityP);

        em.getTransaction().begin();
        final OWLClassP toUpdate = em.find(OWLClassP.class, entityP.getUri());
        for (Set<Object> set : toUpdate.getProperties().values()) {
            final Iterator<Object> it = set.iterator();
            while (it.hasNext()) {
                it.next();
                if (Generators.randomBoolean()) {
                    it.remove();
                }
            }
        }
        em.getTransaction().commit();
        toUpdate.getProperties().keySet().removeIf(property -> toUpdate.getProperties().get(property).isEmpty());
        if (toUpdate.getProperties().isEmpty()) {
            toUpdate.setProperties(null);
        }

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertEquals(toUpdate.getProperties(), res.getProperties());
    }

    @Test
    public void testRemoveAllValuesOfTypedUnmappedProperty() {
        this.em = getEntityManager("RemoveAllValuesOfUnmappedPropertyTyped", false);
        entityP.setProperties(Generators.createTypedProperties(15));
        persist(entityP);

        final OWLClassP toUpdate = em.find(OWLClassP.class, entityP.getUri());
        em.detach(toUpdate);
        // Copy the keys to prevent concurrent modification
        final Set<URI> keys = new HashSet<>(toUpdate.getProperties().keySet());
        keys.stream().filter(k -> Generators.randomBoolean())
            .forEach(key -> toUpdate.getProperties().remove(key));
        em.getTransaction().begin();
        final OWLClassP merged = em.merge(toUpdate);
        assertTrue(TestEnvironmentUtils.arePropertiesEqual(toUpdate.getProperties(), merged.getProperties()));
        em.getTransaction().commit();

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(res);
        assertTrue(TestEnvironmentUtils.arePropertiesEqual(toUpdate.getProperties(), res.getProperties()));
    }

    @Test
    public void testRemoveAllValuesOfPluralPlainIdentifierObjectProperty() {
        this.em = getEntityManager("RemoveAllValuesOfPluralPlainIdentifierOP", false);
        entityP.setIndividuals(Generators.createUrls());
        persist(entityP);

        final OWLClassP toUpdate = em.find(OWLClassP.class, entityP.getUri());
        em.getTransaction().begin();
        toUpdate.getIndividuals().clear();
        em.getTransaction().commit();

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertNull(res.getIndividuals());
    }

    @Test
    public void testSetAnnotationPropertyValueToNull() {
        this.em = getEntityManager("SetAnnotationPropertyValueToNull", false);
        entityN.setAnnotationProperty("annotationPropertyValue");
        persist(entityN);

        final OWLClassN update = em.find(OWLClassN.class, entityN.getId());
        assertNotNull(update.getAnnotationProperty());
        em.getTransaction().begin();
        update.setAnnotationProperty(null);
        em.getTransaction().commit();

        final OWLClassN res = em.find(OWLClassN.class, entityN.getId());
        assertNull(res.getAnnotationProperty());
    }

    @Test
    public void testSetAnnotationPropertyValueContainingUriToNull() {
        this.em = getEntityManager("SetAnnotationPropertyValueContainingUriToNull", false);
        entityN.setAnnotationUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#annotationPropertyValue"));
        persist(entityN);

        final OWLClassN update = em.find(OWLClassN.class, entityN.getId());
        assertNotNull(update.getAnnotationUri());
        em.getTransaction().begin();
        update.setAnnotationUri(null);
        em.getTransaction().commit();

        final OWLClassN res = em.find(OWLClassN.class, entityN.getId());
        assertNull(res.getAnnotationUri());
    }

    @Test
    public void testClearUriTypes() {
        this.em = getEntityManager("ClearUriTypes", false);
        entityP.setTypes(Generators.createUriTypes());
        persist(entityP);

        em.getTransaction().begin();
        final OWLClassP update = em.find(OWLClassP.class, entityP.getUri());
        update.getTypes().clear();
        em.getTransaction().commit();

        final OWLClassP result = em.find(OWLClassP.class, entityP.getUri());
        assertNull(result.getTypes());
    }

    @Test
    public void testRemoveDetachedWithCascadedReferenceUsingMergeAndRemove() {
        this.em = getEntityManager("RemoveDetachedEntityWithCascadedReferenceUsingMergeAndRemove", true);
        persist(entityH);
        assertNotNull(em.find(OWLClassH.class, entityH.getUri()));
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
        final OWLClassH h = new OWLClassH();
        h.setUri(entityH.getUri());
        final OWLClassA a = new OWLClassA();
        a.setUri(entityA.getUri());
        a.setStringAttribute(entityA.getStringAttribute());
        a.setTypes(new HashSet<>(entityA.getTypes()));
        h.setOwlClassA(a);

        em.clear();
        em.getTransaction().begin();
        final OWLClassH toRemove = em.merge(h);
        em.remove(toRemove);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassH.class, entityH.getUri()));
        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void removeEntityTwiceInOneTransactionRemovesIt() {
        this.em = getEntityManager("RemoveDetachedEntityWithCascadedReferenceUsingMergeAndRemove", true);
        persist(entityA);

        em.getTransaction().begin();
        final OWLClassA toRemove = em.merge(entityA);
        em.remove(toRemove);
        assertFalse(em.contains(toRemove));
        em.remove(toRemove);
        assertFalse(em.contains(toRemove));
        em.getTransaction().commit();

        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void settingDatatypeCollectionToNullRemovesAllValues() {
        this.em = getEntityManager("settingDatatypeCollectionToNullRemovesAllValues", true);
        persist(entityM);

        em.getTransaction().begin();
        final OWLClassM toClear = em.find(OWLClassM.class, entityM.getKey());
        toClear.setIntegerSet(null);
        em.getTransaction().commit();

        final OWLClassM result = em.find(OWLClassM.class, entityM.getKey());
        assertNull(result.getIntegerSet());
        verifyDatatypePropertiesRemoved();
    }

    private void verifyDatatypePropertiesRemoved() {
        for (Integer value : entityM.getIntegerSet()) {
            assertFalse(em.createNativeQuery("ASK { ?x ?p ?v . }", Boolean.class)
                          .setParameter("x", URI.create(entityM.getKey()))
                          .setParameter("p", URI.create(Vocabulary.p_m_IntegerSet)).setParameter("v", value)
                          .getSingleResult());
        }
    }

    @Test
    public void clearingDatatypeCollectionRemovesAllValues() {
        this.em = getEntityManager("clearingDatatypeCollectionRemovesAllValues", true);
        persist(entityM);

        em.getTransaction().begin();
        final OWLClassM toClear = em.find(OWLClassM.class, entityM.getKey());
        toClear.getIntegerSet().clear();
        em.getTransaction().commit();

        final OWLClassM result = em.find(OWLClassM.class, entityM.getKey());
        // Could be the cached variant, which contains empty collection, or loaded from ontology, which contains null
        assertTrue(result.getIntegerSet() == null || result.getIntegerSet().isEmpty());
        verifyDatatypePropertiesRemoved();
    }

    @Test
    public void removingNewlyPersistedInstanceRemovesPendingPersistsAndAllowsTransactionToFinish() {
        this.em = getEntityManager("removingNewlyPersistedInstanceRemovesPendingPersistsAndAllowsTransactionToFinish",
                true);
        em.getTransaction().begin();
        em.persist(entityD);
        em.remove(entityD);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassD.class, entityD.getUri()));
        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void removingNewlyPersistedInstanceRemovesPendingListReferencesAndAllowsTransactionToFinish() {
        this.em = getEntityManager(
                "removingNewlyPersistedInstanceRemovesPendingListReferencesAndAllowsTransactionToFinish", true);
        em.getTransaction().begin();
        entityC.setSimpleList(Generators.createSimpleList());
        entityC.setReferencedList(Generators.createReferencedList());
        em.persist(entityC);
        em.remove(entityC);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassC.class, entityC.getUri()));
        entityC.getSimpleList().forEach(a -> assertNull(em.find(OWLClassA.class, a.getUri())));
        entityC.getReferencedList().forEach(a -> assertNull(em.find(OWLClassA.class, a.getUri())));
    }

    @Test
    public void removingListItemsFromNewlyPersistedOwnerRemovesThemFromPendingReferencesAndAllowsTransactionToFinish() {
        this.em = getEntityManager(
                "removingListItemsFromNewlyPersistedOwnerRemovesThemFromPendingReferencesAndAllowsTransactionToFinish",
                false);
        em.getTransaction().begin();
        entityC.setSimpleList(Generators.createSimpleList());
        entityC.setReferencedList(Generators.createReferencedList());
        em.persist(entityC);
        entityC.getSimpleList().clear();
        entityC.getReferencedList().clear();
        em.getTransaction().commit();

        final OWLClassC result = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(result);
        assertNull(result.getSimpleList());
        assertNull(result.getReferencedList());
    }
}
