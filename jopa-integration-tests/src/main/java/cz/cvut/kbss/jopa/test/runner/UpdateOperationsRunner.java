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

import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.oom.exceptions.UnpersistedChangeException;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import org.junit.Test;
import org.slf4j.Logger;

import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.core.Is.isA;
import static org.junit.Assert.*;

public abstract class UpdateOperationsRunner extends BaseRunner {

    private OWLClassA entityA2;
    private OWLClassF entityF;
    private OWLClassJ entityJ;
    private OWLClassO entityO;

    public UpdateOperationsRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
        initialize();
    }

    private void initialize() {
        this.entityF = new OWLClassF();
        entityF.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityF"));
        this.entityA2 = new OWLClassA();
        entityA2.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA2"));
        this.entityJ = new OWLClassJ();
        entityJ.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityJ"));
        final Set<OWLClassA> set = new HashSet<>(2);
        set.add(entityA);
        set.add(entityA2);
        entityJ.setOwlClassA(set);
        this.entityO = new OWLClassO(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityO"));
        final Set<OWLClassE> es = new HashSet<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassE e = new OWLClassE();
            e.setStringAttribute("StringAttributeNo" + i);
            es.add(e);
        }
        entityO.setOwlClassESet(es);
    }

    @Test
    public void testUpdateDataLeaveLazy() throws Exception {
        logger.debug(
                "Test: update data property. Leaves lazily loaded field empty and checks that after commit the field's value hasn't changed.");
        this.em = getEntityManager("UpdateDataProperty", false);
        entityB.setProperties(Generators.createProperties());
        persist(entityB);

        em.getTransaction().begin();
        final OWLClassB b = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(b);
        final Field propsField = OWLClassB.class.getDeclaredField("properties");
        propsField.setAccessible(true);
        assertNull(propsField.get(b));
        final String newString = "NewString";
        b.setStringAttribute(newString);
        em.getTransaction().commit();

        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(res);
        assertEquals(newString, res.getStringAttribute());
        assertNotNull(res.getProperties());
        assertEquals(entityB.getProperties(), res.getProperties());
    }

    @Test
    public void testUpdateDataPropertySetNull() {
        logger.debug("Test: update data property. Set it to null.");
        this.em = getEntityManager("UpdateDataPropertyToNull", true);
        persist(entityA);

        em.getTransaction().begin();
        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        assertNotNull(a.getStringAttribute());
        a.setStringAttribute(null);
        em.getTransaction().commit();

        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(res);
        assertNull(res.getStringAttribute());
        assertEquals(entityA.getTypes(), res.getTypes());
    }

    @Test
    public void testUpdateReference() {
        logger.debug("Test: update reference to entity.");
        this.em = getEntityManager("UpdateReference", true);
        persist(entityD, entityI);
        // em.persist(entityA, ctx);

        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newEntityA"));
        newA.setStringAttribute("newA");
        em.getTransaction().begin();
        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
        assertNotNull(d.getOwlClassA());
        d.setOwlClassA(newA);
        final OWLClassI i = em.find(OWLClassI.class, entityI.getUri());
        assertNotNull(i.getOwlClassA());
        i.setOwlClassA(newA);
        em.persist(newA);
        em.getTransaction().commit();

        final OWLClassA resA1 = em.find(OWLClassA.class, newA.getUri());
        assertNotNull(resA1);
        final OWLClassD resD = em.find(OWLClassD.class, d.getUri());
        assertEquals(resA1.getUri(), resD.getOwlClassA().getUri());
//		assertSame(resD.getOwlClassA(), resA1);
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
        final OWLClassI resI = em.find(OWLClassI.class, i.getUri());
        assertEquals(newA.getUri(), resI.getOwlClassA().getUri());
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
        assertEquals(resA1.getUri(), resI.getOwlClassA().getUri());
//		assertEquals(resA1, resI.getOwlClassA());
    }

    @Test
    public void mergedInstanceWithChangesInCascadedPluralReferenceAttributeContainsUpdatedValues() {
        this.em = getEntityManager("MergeSet", false);
        persist(entityJ);
        em.clear();

        entityJ.getOwlClassA().forEach(a -> a.setStringAttribute("NEWVALUE"));

        em.getTransaction().begin();
        OWLClassJ merged = em.merge(entityJ);

        for (final OWLClassA a : merged.getOwlClassA()) {
            assertEquals(a.getStringAttribute(), "NEWVALUE");
        }
        em.getTransaction().commit();
    }

    @Test
    public void testMergeDetachedWithChanges() {
        logger.debug("Test: merge detached entity with changes.");
        this.em = getEntityManager("UpdateDetached", true);
        persist(entityA);

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertTrue(em.contains(a));
        em.detach(a);
        assertFalse(em.contains(a));
        final String newType = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#AddedType";
        a.getTypes().add(newType);
        em.getTransaction().begin();
        final OWLClassA merged = em.merge(a);
        assertTrue(merged.getTypes().contains(newType));
        em.getTransaction().commit();

        final OWLClassA res = em.find(OWLClassA.class, a.getUri());
        assertEquals(a.getTypes().size(), res.getTypes().size());
        assertTrue(res.getTypes().containsAll(a.getTypes()));
    }

    @Test
    public void testMergeDetachedCascade() {
        logger.debug("Test: merge detached with cascade.");
        this.em = getEntityManager("UpdateCascade", true);
        em.getTransaction().begin();
        em.persist(entityH);
        assertTrue(em.contains(entityA));
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassH h = em.find(OWLClassH.class, entityH.getUri());
        assertNotNull(h.getOwlClassA());
        em.detach(h);
        assertFalse(em.contains(h));
        assertFalse(em.contains(h.getOwlClassA()));
        final String newStr = "newStringAttribute";
        h.getOwlClassA().setStringAttribute(newStr);
        final OWLClassH merged = em.merge(h);
        assertEquals(newStr, merged.getOwlClassA().getStringAttribute());
        em.getTransaction().commit();

        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(res);
        assertEquals(newStr, res.getStringAttribute());
    }

    @Test
    public void testMergeDetachedWithObjectPropertyChange() {
        logger.debug("Test: merge detached with object property change.");
        this.em = getEntityManager("UpdateDetachedWithOPChange", true);
        persist(entityD, entityA);

        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
        em.detach(d);
        d.setOwlClassA(entityA2);
        em.getTransaction().begin();
        em.merge(d);
        em.persist(entityA2);
        em.getTransaction().commit();

        final OWLClassD res = em.find(OWLClassD.class, entityD.getUri());
        assertNotNull(res);
        assertEquals(entityA2.getUri(), res.getOwlClassA().getUri());
        assertEquals(entityA2.getStringAttribute(), res.getOwlClassA().getStringAttribute());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(resA);
        assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
        assertEquals(entityA.getTypes(), resA.getTypes());
    }

    @Test
    public void testRemoveFromSimpleList() {
        logger.debug("Test: remove entity from simple list. (But keep it in the ontology.)");
        this.em = getEntityManager("UpdateRemoveFromSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        persistEntityWithList();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        final OWLClassA a = c.getSimpleList().get(1);
        c.getSimpleList().remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNotNull(resA);
        final OWLClassC resC = em.find(OWLClassC.class, c.getUri());
        assertEquals(c.getSimpleList().size(), resC.getSimpleList().size());
        assertEquals(entityC.getSimpleList().size() - 1, resC.getSimpleList().size());
        for (OWLClassA aa : resC.getSimpleList()) {
            assertNotEquals(resA.getUri(), aa.getUri());
        }
    }

    private void persistEntityWithList() {
        em.getTransaction().begin();
        em.persist(entityC);
        if (entityC.getSimpleList() != null) {
            entityC.getSimpleList().forEach(em::persist);
        }
        if (entityC.getReferencedList() != null) {
            entityC.getReferencedList().forEach(em::persist);
        }
        em.getTransaction().commit();
    }

    @Test
    public void testAddToSimpleList() {
        logger.debug("Test: add entity to simple list.");
        this.em = getEntityManager("UpdateAddToSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.persist(entityA);
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        assertFalse(c.getSimpleList().contains(a));
        c.getSimpleList().add(a);
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        assertEquals(c.getSimpleList().size(), resC.getSimpleList().size());
        assertEquals(entityC.getSimpleList().size() + 1, resC.getSimpleList().size());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(resA);
        assertTrue(resC.getSimpleList().contains(resA));
    }

    @Test
    public void testClearSimpleList() {
        logger.debug("Test: clear a simple list (but keep the entities in ontology).");
        this.em = getEntityManager("UpdateClearSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        persistEntityWithList();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        assertFalse(c.getSimpleList().isEmpty());
        em.getTransaction().begin();
        c.getSimpleList().clear();
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(resC);
        assertTrue(resC.getSimpleList() == null || resC.getSimpleList().isEmpty());
        for (OWLClassA a : entityC.getSimpleList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
    }

    @Test
    public void testReplaceSimpleList() {
        logger.debug("Test: replace simple list with a new one.");
        this.em = getEntityManager("UpdateReplaceSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        persistEntityWithList();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        final List<OWLClassA> newList = new ArrayList<>(1);
        newList.add(entityA);
        em.getTransaction().begin();
        em.persist(entityA);
        c.setSimpleList(newList);
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(resC);
        assertEquals(newList.size(), resC.getSimpleList().size());
        boolean found;
        for (OWLClassA a : newList) {
            found = false;
            for (OWLClassA aa : resC.getSimpleList()) {
                if (a.getUri().equals(aa.getUri())) {
                    found = true;
                    break;
                }
            }
            assertTrue(found);
        }
        for (OWLClassA a : entityC.getSimpleList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
    }

    @Test
    public void testRemoveFromReferencedList() {
        logger.debug("Test: remove entity from referenced list. (But keep it in the ontology.");
        this.em = getEntityManager("UpdateRemoveFromReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        persistEntityWithList();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        final OWLClassA a = c.getReferencedList().get(Generators.randomInt(c.getReferencedList().size()));
        c.getReferencedList().remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNotNull(resA);
        final OWLClassC resC = em.find(OWLClassC.class, c.getUri());
        assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
        assertEquals(entityC.getReferencedList().size() - 1, resC.getReferencedList().size());
        for (OWLClassA aa : resC.getReferencedList()) {
            assertNotEquals(resA.getUri(), aa.getUri());
        }
    }

    @Test
    public void testAddToReferencedList() {
        logger.debug("Test: add entity to Referenced list.");
        this.em = getEntityManager("UpdateAddToReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        persistEntityWithList();

        em.getTransaction().begin();
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.persist(entityA);
        c.getReferencedList().add(entityA);
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
        assertEquals(entityC.getReferencedList().size() + 1, resC.getReferencedList().size());
        final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(resA);
        assertTrue(resC.getReferencedList().contains(resA));
    }

    @Test
    public void testClearReferencedList() {
        logger.debug("Test: clear referenced list (but keep the entities in ontology).");
        this.em = getEntityManager("UpdateClearReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        persistEntityWithList();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        assertFalse(c.getReferencedList().isEmpty());
        em.getTransaction().begin();
        c.setReferencedList(null);
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(resC);
        assertNull(resC.getReferencedList());
        for (OWLClassA a : entityC.getReferencedList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
    }

    @Test
    public void testReplaceReferencedList() {
        logger.debug("Test: replace referenced list with a new one.");
        this.em = getEntityManager("UpdateReplaceReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        persistEntityWithList();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        final List<OWLClassA> newList = new ArrayList<>(1);
        newList.add(entityA);
        em.getTransaction().begin();
        em.persist(entityA);
        c.setReferencedList(newList);
        em.getTransaction().commit();

        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(resC);
        assertEquals(newList.size(), resC.getReferencedList().size());
        boolean found;
        for (OWLClassA a : newList) {
            found = false;
            for (OWLClassA aa : resC.getReferencedList()) {
                if (a.getUri().equals(aa.getUri())) {
                    found = true;
                    break;
                }
            }
            assertTrue(found);
        }
        for (OWLClassA a : entityC.getReferencedList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
    }

    @Test
    public void testAddNewToProperties() {
        logger.debug("Test: add a new property value to entity's properties.");
        this.em = getEntityManager("UpdateAddNewToProperties", false);
        entityB.setProperties(Generators.createProperties());
        final Map<String, Set<String>> expected = new HashMap<>(entityB.getProperties().size() + 3);
        expected.putAll(entityB.getProperties());
        persist(entityB);
        em.clear();

        final OWLClassB b = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(b);
        assertEquals(expected.size(), b.getProperties().size());
        em.getTransaction().begin();
        b.getProperties().put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyFour",
                Collections.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/Stroustrup"));
        expected.putAll(b.getProperties());
        em.getTransaction().commit();

        final OWLClassB res = em.find(OWLClassB.class, b.getUri());
        assertNotNull(res);
        assertEquals(expected.size(), res.getProperties().size());
        for (Map.Entry<String, Set<String>> e : expected.entrySet()) {
            assertTrue(res.getProperties().containsKey(e.getKey()));
            final Set<String> s = e.getValue();
            final Set<String> resS = res.getProperties().get(e.getKey());
            assertEquals(s.size(), resS.size());
            assertTrue(s.containsAll(resS));
        }
    }

    @Test
    public void testAddPropertyValue() {
        logger.debug("Test: add another value to an existing property.");
        this.em = getEntityManager("UpdateAddPropertyValue", false);
        entityB.setProperties(Generators.createProperties());
        final Map<String, Set<String>> expected = new HashMap<>(entityB.getProperties().size() + 3);
        final String prop = entityB.getProperties().keySet().iterator().next();
        expected.putAll(entityB.getProperties());
        persist(entityB);
        em.clear();

        final OWLClassB b = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(b);
        assertEquals(expected.size(), b.getProperties().size());
        em.getTransaction().begin();
        b.getProperties().get(prop).add("http://krizik.felk.cvut.cz/ontologies/jopa/Stroustrup");
        expected.putAll(b.getProperties());
        em.getTransaction().commit();

        final OWLClassB res = em.find(OWLClassB.class, b.getUri());
        assertNotNull(res);
        assertEquals(expected.size(), res.getProperties().size());
        for (Map.Entry<String, Set<String>> e : expected.entrySet()) {
            assertTrue(res.getProperties().containsKey(e.getKey()));
            final Set<String> s = e.getValue();
            final Set<String> resS = res.getProperties().get(e.getKey());
            assertEquals(s.size(), resS.size());
            if (e.getKey().equals(prop)) {
                assertTrue(s.size() > 1);
                for (String val : s) {
                    assertTrue(resS.contains(val));
                }
            } else {
                assertTrue(resS.contains(s.iterator().next()));
            }
        }
    }

    @Test
    public void testAddPropertyValueDetached() {
        logger.debug("Test: add another value to an existing property.");
        this.em = getEntityManager("UpdateAddPropertyValueDetached", false);
        entityB.setProperties(Generators.createProperties());
        final String prop = entityB.getProperties().keySet().iterator().next();
        final String newPropertyValue = "http://krizik.felk.cvut.cz/ontologies/jopa#newPropertyValue";
        persist(entityB);
        em.clear();

        final OWLClassB b = em.find(OWLClassB.class, entityB.getUri());
        b.getProperties().get(prop).add(newPropertyValue);
        em.detach(b);
        em.getTransaction().begin();
        em.merge(b);
        em.getTransaction().commit();

        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(res);
        assertTrue(res.getProperties().get(prop).contains(newPropertyValue));
    }

    @Test
    public void testAddNewTypedPropertyWithValues() {
        logger.debug("Test: add a new property with values. Typed.");
        this.em = getEntityManager("UpdateAddNewToPropertiesTyped", false);
        entityP.setProperties(Generators.createTypedProperties());
        persist(entityP);
        em.clear();

        final OWLClassP p = em.find(OWLClassP.class, entityP.getUri());
        em.getTransaction().begin();
        final URI property = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#newProperty");
        p.getProperties().put(property, new HashSet<>(Arrays.asList(1, "Two", new Date())));
        em.getTransaction().commit();

        em.clear();

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(res);
        assertTrue(res.getProperties().containsKey(property));
        assertEquals(p.getProperties(), res.getProperties());
    }

    @Test
    public void testAddTypedPropertyValue() {
        logger.debug("Test: add an existing property value. Typed.");
        this.em = getEntityManager("UpdateAddPropertyValueTyped", false);
        entityP.setProperties(Generators.createTypedProperties());
        persist(entityP);
        em.clear();

        final OWLClassP p = em.find(OWLClassP.class, entityP.getUri());
        em.getTransaction().begin();
        final URI property = p.getProperties().keySet().iterator().next();
        final Object value = generateValueForProperty(p, property);
        p.getProperties().get(property).add(value);
        em.getTransaction().commit();

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertTrue(res.getProperties().get(property).contains(value));
        assertEquals(p.getProperties(), res.getProperties());
    }

    private Object generateValueForProperty(OWLClassP instance, URI property) {
        final Set<Object> values = instance.getProperties().get(property);
        assert values != null && !values.isEmpty();
        if (values.iterator().next() instanceof URI) {
            return URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#newPropertyValue");
        } else {
            return "StringValue";
        }
    }

    @Test(expected = OWLInferredAttributeModifiedException.class)
    public void testModifyInferredAttribute() {
        logger.debug("Test: modify an inferred attribute.");
        this.em = getEntityManager("ModifyInferredAttribute", false);
        persist(entityF);

        em.getTransaction().begin();
        final OWLClassF f = em.find(OWLClassF.class, entityF.getUri());
        assertNotNull(f);
        f.setSecondStringAttribute("otherValue");
        em.getTransaction().commit();
    }

    @Test
    public void testModifyAttributesOfBasicTypes() {
        logger.debug("Test: modify attributes of basic Java types (Integer, Boolean etc.).");
        this.em = getEntityManager("ModifyBasicTypeAttributes", false);
        persist(entityM);

        em.getTransaction().begin();
        final OWLClassM m = em.find(OWLClassM.class, entityM.getKey());
        m.setBooleanAttribute(!entityM.getBooleanAttribute());
        m.setDoubleAttribute(m.getDoubleAttribute() - 100.0);
        m.setLongAttribute(m.getLongAttribute() + 100L);
        m.setDateAttribute(new Date(System.currentTimeMillis() + 10000));
        em.getTransaction().commit();

        final OWLClassM res = em.find(OWLClassM.class, entityM.getKey());
        assertEquals(m.getBooleanAttribute(), res.getBooleanAttribute());
        assertEquals(m.getLongAttribute(), res.getLongAttribute());
        assertEquals(m.getDoubleAttribute(), res.getDoubleAttribute());
        assertEquals(m.getDateAttribute(), res.getDateAttribute());
    }

    @Test
    public void testModifyEnumAttribute() {
        logger.debug("Test: modify enum attribute.");
        this.em = getEntityManager("ModifyEnumAttribute", false);
        assertNotNull(entityM.getEnumAttribute());
        persist(entityM);

        final OWLClassM.Severity updated = OWLClassM.Severity.LOW;
        em.getTransaction().begin();
        final OWLClassM m = em.find(OWLClassM.class, entityM.getKey());
        m.setEnumAttribute(updated);
        em.getTransaction().commit();

        final OWLClassM res = em.find(OWLClassM.class, entityM.getKey());
        assertNotNull(res);
        assertEquals(updated, res.getEnumAttribute());
    }

    @Test
    public void testCascadeOfNewlyPersistedOnMerge() {
        logger.debug("Test: cascade merging of an object that has been just persisted.");
        this.em = getEntityManager("CascadeMergeWithNewlyPersisted", true);
        em.getTransaction().begin();
        em.persist(entityO);
        entityO.getOwlClassESet().forEach(em::persist);
        em.getTransaction().commit();

        final OWLClassE newE = new OWLClassE();
        newE.setStringAttribute("NewlyAddedE");
        final OWLClassO toUpdate = em.find(OWLClassO.class, entityO.getUri());
        em.detach(toUpdate);

        em.getTransaction().begin();
        toUpdate.getOwlClassESet().add(newE);
        em.persist(newE);
        final OWLClassO merged = em.merge(toUpdate);
        assertTrue(merged.getOwlClassESet().contains(newE));
        em.getTransaction().commit();

        final OWLClassE resE = em.find(OWLClassE.class, newE.getUri());
        assertNotNull(resE);
        final OWLClassO resO = em.find(OWLClassO.class, entityO.getUri());
        assertEquals(entityO.getOwlClassESet().size() + 1, resO.getOwlClassESet().size());
    }

    @Test
    public void modificationsOfCollectionAfterCascadeMergeAreWrittenOnCommit() {
        logger.debug(
                "Test: modify collection after cascade merge and check that the changes have been propagated on commit.");
        this.em = getEntityManager("ModifyCollectionAfterCascadeMerge", true);
        em.getTransaction().begin();
        em.persist(entityJ);
        em.getTransaction().commit();

        final OWLClassA newA = new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newEntityA"));
        em.getTransaction().begin();
        final OWLClassJ merged = em.merge(entityJ);
        merged.getOwlClassA().add(newA);
        em.persist(newA);
        em.getTransaction().commit();

        final OWLClassJ result = em.find(OWLClassJ.class, entityJ.getUri());
        assertEquals(merged.getOwlClassA().size(), result.getOwlClassA().size());
        boolean found = false;
        for (OWLClassA a : result.getOwlClassA()) {
            if (a.getUri().equals(newA.getUri())) {
                found = true;
                break;
            }
        }
        assertTrue(found);
    }

    @Test
    public void testAddPropertiesWhenTheyWereNullOriginally() {
        logger.debug("Test: add properties on update when the field was originally null.");
        this.em = getEntityManager("AddPropertiesToNullOriginals", true);
        em.getTransaction().begin();
        assertNull(entityB.getProperties());
        em.persist(entityB);
        em.getTransaction().commit();

        final OWLClassB update = em.find(OWLClassB.class, entityB.getUri());
        em.detach(update);
        final Map<String, Set<String>> properties = Generators.createProperties(2);
        update.setProperties(properties);
        em.getTransaction().begin();
        em.merge(update);
        em.getTransaction().commit();

        final OWLClassB result = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(result.getProperties());
        assertEquals(properties, result.getProperties());
    }

    @Test
    public void testUpdatePlainLiteralObjectPropertyValueToAnotherIndividual() {
        logger.debug("Test: update plain literal object property value to a different individual.");
        this.em = getEntityManager("UpdatePlainLiteralObjectPropertyValue", true);
        em.getTransaction().begin();
        entityP.setIndividualUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#oldIndividual"));
        em.persist(entityP);
        em.getTransaction().commit();

        final OWLClassP update = em.find(OWLClassP.class, entityP.getUri());
        final URI newValue = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#newIndividual");
        em.getTransaction().begin();
        update.setIndividualUri(newValue);
        em.getTransaction().commit();

        final OWLClassP result = em.find(OWLClassP.class, entityP.getUri());
        assertEquals(newValue, result.getIndividualUri());
    }

    @Test
    public void testUpdatePlainLiteralObjectPropertyValueToNull() {
        logger.debug("Test: update plain literal object property value to null.");
        this.em = getEntityManager("UpdatePlainLiteralObjectPropertyValueToNull", true);
        em.getTransaction().begin();
        entityP.setIndividualUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#SomeIndividual"));
        em.persist(entityP);
        em.getTransaction().commit();

        final OWLClassP update = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(update.getIndividualUri());
        em.getTransaction().begin();
        update.setIndividualUri(null);
        em.getTransaction().commit();

        final OWLClassP result = em.find(OWLClassP.class, entityP.getUri());
        assertNull(result.getIndividualUri());
    }

    @Test
    public void testUpdatePluralPlainLiteralObjectPropertyAttribute() throws MalformedURLException {
        logger.debug("Test: update plural plain literal object property attribute.");
        this.em = getEntityManager("UpdatePluralPlainLiteralObjectPropertyValue", true);
        entityP.setIndividuals(Generators.createUrls());
        persist(entityP);

        final OWLClassP update = em.find(OWLClassP.class, entityP.getUri());
        em.getTransaction().begin();
        final URL added = new URL("http://krizik.felk.cvut.cz/ontologies/jopa#added");
        final URL removed = entityP.getIndividuals().iterator().next();
        update.getIndividuals().add(added);
        update.getIndividuals().remove(removed);
        em.getTransaction().commit();

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(res);
        assertEquals(update.getIndividuals(), res.getIndividuals());
    }

    @Test
    public void testUpdateSimpleListOfIdentifiersByAddingNewItems() {
        this.em = getEntityManager("UpdateSimpleListOfIdentifiersByAddingItems", true);
        entityP.setSimpleList(Generators.createListOfIdentifiers());
        persist(entityP);

        final OWLClassP update = em.find(OWLClassP.class, entityP.getUri());
        em.getTransaction().begin();
        for (int i = 0; i < Generators.randomPositiveInt(5, 10); i++) {
            final URI u = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#Added-" + i);
            // Insert at random position
            update.getSimpleList().add(Generators.randomInt(update.getSimpleList().size()), u);
        }
        em.getTransaction().commit();

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(res);
        assertEquals(update.getSimpleList(), res.getSimpleList());
    }

    @Test
    public void testUpdateReferencedListByRemovingAndAddingItems() {
        this.em = getEntityManager("UpdateReferencedListByRemovingAndAddingItems", true);
        entityP.setReferencedList(Generators.createListOfIdentifiers());
        persist(entityP);

        final OWLClassP update = em.find(OWLClassP.class, entityP.getUri());
        em.getTransaction().begin();
        for (int i = 0; i < Generators.randomPositiveInt(5, 10); i++) {
            final URI u = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#Added-" + i);
            // We might even overwrite items set in previous iterations, but it does not matter. JOPA should handle it
            update.getReferencedList().set(Generators.randomInt(update.getReferencedList().size()), u);
        }
        em.getTransaction().commit();

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(res);
        assertEquals(update.getReferencedList(), res.getReferencedList());
    }

    @Test
    public void testUpdateStringAnnotationPropertyValue() {
        this.em = getEntityManager("UpdateStringAnnotationPropertyValue", true);
        entityN.setAnnotationProperty("PropertyValue");
        persist(entityN);

        final String newValue = "newValue";
        final OWLClassN update = em.find(OWLClassN.class, entityN.getId());
        assertNotNull(update);
        em.getTransaction().begin();
        update.setAnnotationProperty(newValue);
        em.getTransaction().commit();

        final OWLClassN res = em.find(OWLClassN.class, entityN.getId());
        assertEquals(newValue, res.getAnnotationProperty());
    }

    @Test
    public void testUpdateAnnotationPropertyWithUriValueToDifferentValue() {
        this.em = getEntityManager("UpdateAnnotationPropertyWithUriValueToDifferentValue", true);
        entityN.setAnnotationUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#value"));
        persist(entityN);

        final OWLClassN update = em.find(OWLClassN.class, entityN.getId());
        assertNotNull(update);
        em.getTransaction().begin();
        final URI newUri = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#newValue");
        update.setAnnotationUri(newUri);
        em.getTransaction().commit();

        final OWLClassN res = em.find(OWLClassN.class, entityN.getId());
        assertEquals(newUri, res.getAnnotationUri());
    }

    @Test
    public void testUpdateUriTypes() {
        this.em = getEntityManager("UpdateUriTypes", true);
        entityP.setTypes(Generators.createUriTypes());
        persist(entityP);

        em.getTransaction().begin();
        final OWLClassP update = em.find(OWLClassP.class, entityP.getUri());
        final Iterator<URI> it = update.getTypes().iterator();
        while (it.hasNext()) {
            it.next();
            if (Generators.randomBoolean()) {
                it.remove();
            }
        }
        update.getTypes().addAll(Generators.createUriTypes());
        em.getTransaction().commit();

        final OWLClassP result = em.find(OWLClassP.class, entityP.getUri());
        assertEquals(update.getTypes().size(), result.getTypes().size());
        assertTrue(update.getTypes().containsAll(result.getTypes()));
    }

    @Test
    public void updateWithInvalidInstanceThrowsIntegrityConstraintsViolation() {
        thrown.expect(RollbackException.class);
        thrown.expectCause(isA(IntegrityConstraintViolatedException.class));

        this.em = getEntityManager("UpdateWithInvalidInstanceThrowsICViolation", true);
        persist(entityN);

        entityN.setStringAttribute(null);
        em.getTransaction().begin();
        em.merge(entityN);
        em.getTransaction().commit();
    }

    @Test
    public void addingValuesToDatatypePropertyCollectionAddsThemIntoRepository() {
        this.em = getEntityManager("addingValuesToDatatypePropertyCollectionAddsThemIntoRepository", false);
        persist(entityM);

        IntStream.generate(Generators::randomInt).limit(7).forEach(entityM.getIntegerSet()::add);
        em.getTransaction().begin();
        em.merge(entityM);
        em.getTransaction().commit();

        final OWLClassM result = em.find(OWLClassM.class, entityM.getKey());
        assertEquals(entityM.getIntegerSet(), result.getIntegerSet());
    }

    @Test
    public void removingValuesFromDatatypePropertyCollectionRemovesThemFromRepository() {
        this.em = getEntityManager("removingValuesFromDatatypePropertyCollectionRemovesThemFromRepository", false);
        persist(entityM);

        final Iterator<Integer> it = entityM.getIntegerSet().iterator();
        while (it.hasNext()) {
            it.next();
            if (Generators.randomBoolean()) {
                it.remove();
            }
        }
        // Make sure there is at least one element
        entityM.getIntegerSet().add(Generators.randomInt());

        em.getTransaction().begin();
        em.merge(entityM);
        em.getTransaction().commit();

        final OWLClassM result = em.find(OWLClassM.class, entityM.getKey());
        assertEquals(entityM.getIntegerSet(), result.getIntegerSet());
    }

    @Test
    public void mergeReplacesReferenceWithInstanceFromRepository() {
        this.em = getEntityManager("mergeReplacesReferenceWithInstanceFromRepository", true);
        persist(entityD, entityA);

        final String originalString = entityA.getStringAttribute();
        entityA.setStringAttribute("updatedStringAttribute");
        em.getTransaction().begin();
        final OWLClassD merged = em.merge(entityD);
        assertEquals(originalString, merged.getOwlClassA().getStringAttribute());
        em.getTransaction().commit();

        final OWLClassA result = em.find(OWLClassA.class, entityA.getUri());
        assertEquals(originalString, result.getStringAttribute());
    }

    @Test
    public void mergeMergesUpdatedDataPropertyFields() {
        this.em = getEntityManager("mergeMergesUpdatedDataPropertyFields", true);
        persist(entityA);
        final Set<String> newTypes =
                Generators.createUriTypes().stream().map(URI::toString).collect(Collectors.toSet());
        entityA.setTypes(newTypes);
        final String newStringAtt = "newStringAttribute";
        entityA.setStringAttribute(newStringAtt);

        em.getTransaction().begin();
        final OWLClassA merged = em.merge(entityA);
        assertEquals(newStringAtt, merged.getStringAttribute());
        assertTrue(TestEnvironmentUtils.typesEqual(newTypes, merged.getTypes()));
        em.getTransaction().commit();

        final OWLClassA result = em.find(OWLClassA.class, entityA.getUri());
        assertEquals(newStringAtt, result.getStringAttribute());
        assertTrue(TestEnvironmentUtils.typesEqual(newTypes, result.getTypes()));
    }

    @Test
    public void mergeMergesUpdateToObjectPropertyField() {
        this.em = getEntityManager("mergeMergesUpdateToObjectPropertyField", true);
        persist(entityD, entityA, entityA2);

        em.getTransaction().begin();
        entityD.setOwlClassA(entityA2);
        final OWLClassD merged = em.merge(entityD);
        assertEquals(entityA2.getUri(), merged.getOwlClassA().getUri());
        em.getTransaction().commit();

        final OWLClassD dResult = em.find(OWLClassD.class, entityD.getUri());
        assertEquals(entityA2.getUri(), dResult.getOwlClassA().getUri());
        final OWLClassA aReference = em.find(OWLClassA.class, entityA2.getUri());
        assertSame(dResult.getOwlClassA(), aReference);
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void updateSupportsSavingStringLiteralWithDifferentLanguageTag() throws Exception {
        this.em = getEntityManager("updateSupportsSavingStringLiteralWithDifferentLanguageTag", false);
        persist(entityA);

        entityA.setStringAttribute("hodnota v cestine");
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(OWLClassA.class.getDeclaredField("stringAttribute"), "cs");
        em.getTransaction().begin();
        em.merge(entityA, descriptor);
        em.getTransaction().commit();

        final OWLClassA resultOne = em.find(OWLClassA.class, entityA.getUri());
        assertNull(resultOne.getStringAttribute());
        em.clear();
        final OWLClassA resultTwo = em.find(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(entityA.getStringAttribute(), resultTwo.getStringAttribute());
    }

    @Test
    public void mergeDetachedRemovesObsoleteDescriptorInSecondLevelCache() {
        this.em = getEntityManager("mergeDetachedReplacesObsoleteDescriptorInSecondLevelCache", true);
        final Descriptor descriptorOne = new EntityDescriptor();
        descriptorOne.setLanguage("en");
        em.getTransaction().begin();
        em.persist(entityA, descriptorOne);
        em.getTransaction().commit();
        assertTrue(em.getEntityManagerFactory().getCache().contains(OWLClassA.class, entityA.getUri(), descriptorOne));

        final Descriptor descriptorTwo = new EntityDescriptor();
        descriptorTwo.setLanguage("cs");
        assertNotEquals(descriptorOne, descriptorTwo);
        entityA.setStringAttribute("cesky");
        em.getTransaction().begin();
        em.merge(entityA, descriptorTwo);
        em.getTransaction().commit();

        assertFalse(em.getEntityManagerFactory().getCache().contains(OWLClassA.class, entityA.getUri(), descriptorOne));
        final OWLClassA result = em.find(OWLClassA.class, entityA.getUri(), descriptorTwo);
        assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
        assertTrue(em.getEntityManagerFactory().getCache().contains(OWLClassA.class, entityA.getUri(), descriptorTwo));
    }

    @Test
    public void mergeUpdatesCacheEventWhenDescriptorContainsOnlyAttributeLanguageSetting() throws Exception {
        this.em = getEntityManager("mergeUpdatesCacheEventWhenDescriptorContainsOnlyAttributeLanguageSetting", true);
        persist(entityA);

        final Descriptor descriptorTwo = new EntityDescriptor();
        descriptorTwo.setAttributeLanguage(OWLClassA.class.getDeclaredField("stringAttribute"), "cs");
        entityA.setStringAttribute("cesky");
        em.getTransaction().begin();
        em.merge(entityA, descriptorTwo);
        em.getTransaction().commit();

        final OWLClassA result = em.find(OWLClassA.class, entityA.getUri(), descriptorTwo);
        assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
        assertTrue(em.getEntityManagerFactory().getCache().contains(OWLClassA.class, entityA.getUri(), descriptorTwo));
    }

    @Test
    public void updateRemovesPendingAssertionWhenItIsReplacedByAnotherValue() {
        this.em = getEntityManager("updateRemovesPendingAssertionWhenItIsReplacedByAnotherValue", true);
        entityD.setOwlClassA(null);
        persist(entityD, entityA2);

        em.getTransaction().begin();
        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
        d.setOwlClassA(entityA);
        d.setOwlClassA(em.find(OWLClassA.class, entityA2.getUri()));
        em.getTransaction().commit();

        final OWLClassD dResult = em.find(OWLClassD.class, entityD.getUri());
        assertNotNull(dResult.getOwlClassA());
        assertEquals(entityA2.getUri(), dResult.getOwlClassA().getUri());
        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void updatesKeepsPendingAssertionPluralAttribute() {
        this.em = getEntityManager("updatesKeepsPendingAssertionPluralAttribute", true);
        final OWLClassL entityL = new OWLClassL(Generators.generateUri());
        entityL.setSet(new HashSet<>());
        entityL.getSet().add(entityA2);
        entityL.setSimpleList(Collections.singletonList(entityA2));
        entityL.setSingleA(entityA2);
        entityL.setUri(Generators.generateUri());
        persist(entityL, entityA2);

        em.getTransaction().begin();
        final OWLClassL inst = em.find(OWLClassL.class, entityL.getUri());
        inst.getSet().add(entityA);
        thrown.expect(RollbackException.class);
        thrown.expectCause(isA(UnpersistedChangeException.class));
        thrown.expectMessage(containsString(entityA.toString()));
        em.getTransaction().commit();
    }

    @Test
    public void mergeCorrectlyUpdatesCacheInCaseOfChangeInReferencedAttributePreviouslyMerged() {
        this.em = getEntityManager("mergeCorrectlyUpdatesCacheInCaseOfChangeInReferencedAttributePreviouslyMerged",
                true);
        persist(entityD, entityA);

        final String newString = "updatedString";
        final OWLClassD dUpdate = new OWLClassD(entityD.getUri());
        final OWLClassA aUpdate = new OWLClassA(entityA.getUri());
        aUpdate.setStringAttribute(newString);
        aUpdate.setTypes(new HashSet<>(entityA.getTypes()));
        dUpdate.setOwlClassA(aUpdate);
        em.clear();
        em.getTransaction().begin();
        final OWLClassD orig = em.find(OWLClassD.class, entityD.getUri());
        em.detach(orig);
        em.find(OWLClassA.class, entityA.getUri());
        em.merge(aUpdate);
        em.merge(dUpdate);
        em.getTransaction().commit();
        final OWLClassD result = em.find(OWLClassD.class, entityD.getUri());
        assertEquals(newString, result.getOwlClassA().getStringAttribute());
    }

    @Test
    public void mergeCorrectlyUpdatesCacheInCaseOfChangeInReferencedAttributeMergedLater() {
        this.em = getEntityManager("mergeCorrectlyUpdatesCacheInCaseOfChangeInReferencedAttributeMergedLater",
                true);
        persist(entityD, entityA);

        final String newString = "updatedString";
        final OWLClassD dUpdate = new OWLClassD(entityD.getUri());
        final OWLClassA aUpdate = new OWLClassA(entityA.getUri());
        aUpdate.setStringAttribute(newString);
        aUpdate.setTypes(new HashSet<>(entityA.getTypes()));
        dUpdate.setOwlClassA(aUpdate);
        em.clear();
        em.getTransaction().begin();
        final OWLClassD orig = em.find(OWLClassD.class, entityD.getUri());
        em.detach(orig);
        em.merge(dUpdate);
        em.merge(aUpdate);
        em.getTransaction().commit();
        final OWLClassD result = em.find(OWLClassD.class, entityD.getUri());
        assertEquals(newString, result.getOwlClassA().getStringAttribute());
    }

    @Test
    public void mergeCorrectlyMergesChangesOnPluralObjectPropertyIntoCache() {
        this.em = getEntityManager("mergeCorrectlyMergesChangesOnPluralObjectPropertyIntoCache", true);
        final OWLClassL entityL = new OWLClassL(Generators.generateUri());
        entityL.setSet(new HashSet<>());
        entityL.getSet().add(entityA);
        entityL.getSet().add(entityA2);
        entityL.setSimpleList(Collections.singletonList(entityA2));
        entityL.setSingleA(entityA2);
        persist(entityL, entityA, entityA2);

        final OWLClassL toUpdate = new OWLClassL(entityL.getUri());
        toUpdate.setSet(new HashSet<>(entityL.getSet()));
        toUpdate.getSet().remove(entityA2);
        toUpdate.setSimpleList(Collections.singletonList(entityA2));
        toUpdate.setSingleA(entityA2);
        em.getTransaction().begin();
        final OWLClassL original = em.find(OWLClassL.class, entityL.getUri());
        assertEquals(2, original.getSet().size());
        for (OWLClassA a : original.getSet()) {
            if (a.getUri().equals(entityA2.getUri())) {
                original.getSet().remove(a);
                break;
            }
        }
        toUpdate.getSet().forEach(a -> em.merge(a));
        em.merge(toUpdate);
        em.getTransaction().commit();

        final OWLClassL result = em.find(OWLClassL.class, entityL.getUri());
        assertEquals(1, result.getSet().size());
    }

    @Test
    public void updateKeepsPendingListReferenceWhenItemIsAddedToIt() {
        this.em = getEntityManager("updateKeepsPendingListReferenceWhenItemIsAddedToIt", true);
        final OWLClassK entityK = new OWLClassK();
        entityK.setSimpleList(generateEInstances(5));
        entityK.setReferencedList(generateEInstances(6));
        em.getTransaction().begin();
        em.persist(entityK);
        entityK.getSimpleList().forEach(e -> {
            assertNull(e.getUri());
            em.persist(e);
        });
        entityK.getReferencedList().forEach(e -> {
            assertNull(e.getUri());
            em.persist(e);
        });
        em.getTransaction().commit();

        final OWLClassE addedSimple = new OWLClassE();
        addedSimple.setStringAttribute("addedSimple");
        final OWLClassE addedReferenced = new OWLClassE();
        addedReferenced.setStringAttribute("addedReferenced");
        em.getTransaction().begin();
        final OWLClassK update = em.find(OWLClassK.class, entityK.getUri());
        update.getSimpleList().add(addedSimple);
        update.getReferencedList().add(addedReferenced);
        assertNull(addedSimple.getUri());
        assertNull(addedReferenced.getUri());
        em.persist(addedSimple);
        em.persist(addedReferenced);
        em.getTransaction().commit();

        final OWLClassK result = em.find(OWLClassK.class, entityK.getUri());
        assertEquals(addedSimple.getUri(), result.getSimpleList().get(result.getSimpleList().size() - 1).getUri());
        assertEquals(addedReferenced.getUri(),
                result.getReferencedList().get(result.getReferencedList().size() - 1).getUri());
    }

    private List<OWLClassE> generateEInstances(int count) {
        return IntStream.range(0, count).mapToObj(i -> {
            final OWLClassE e = new OWLClassE();
            e.setStringAttribute("instance" + i);
            return e;
        }).collect(Collectors.toList());
    }

    @Test
    public void mergeRemovedThrowsIllegalArgumentException() {
        this.em = getEntityManager("mergeRemovedThrowsIllegalArgument", true);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(containsString("removed"));
        persist(entityA);

        em.getTransaction().begin();
        final OWLClassA toRemove = em.find(OWLClassA.class, entityA.getUri());
        em.remove(toRemove);
        em.merge(toRemove);
    }

    @Test
    public void changesDoneAfterInstanceRefreshAreCommittedToRepository() {
        this.em = getEntityManager("changesDoneAfterInstanceRefreshAreCommittedToRepository", true);
        persist(entityD, entityA, entityA2);

        em.getTransaction().begin();
        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
        final OWLClassA origA = d.getOwlClassA();
        final OWLClassA newA = new OWLClassA(Generators.generateUri());
        newA.setStringAttribute("newAWithStringAttribute");
        d.setOwlClassA(em.find(OWLClassA.class, entityA2.getUri()));
        em.refresh(d);
        assertEquals(origA, d.getOwlClassA());
        d.setOwlClassA(newA);
        em.persist(newA);
        em.getTransaction().commit();

        final OWLClassD result = em.find(OWLClassD.class, entityD.getUri());
        assertEquals(newA.getUri(), result.getOwlClassA().getUri());
        assertEquals(newA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
    }

    @Test
    public void refreshOfPluralObjectPropertyReplacesCollectionWithOriginalAndAllowsFurtherChanges() {
        this.em = getEntityManager("refreshOfPluralObjectPropertyReplacesCollectionWithOriginalAndAllowsFurtherChanges",
                true);
        persist(entityJ);

        em.getTransaction().begin();
        final OWLClassJ j = em.find(OWLClassJ.class, entityJ.getUri());
        j.getOwlClassA().clear();
        em.refresh(j);
        final OWLClassA newA = new OWLClassA(Generators.generateUri());
        newA.setStringAttribute("newAWithStringAttribute");
        j.getOwlClassA().add(newA);
        em.persist(newA);
        em.getTransaction().commit();

        final OWLClassJ result = em.find(OWLClassJ.class, entityJ.getUri());
        assertEquals(entityJ.getOwlClassA().size() + 1, result.getOwlClassA().size());
        final Set<URI> aUris = new HashSet<>(Arrays.asList(entityA.getUri(), entityA2.getUri(), newA.getUri()));
        result.getOwlClassA().forEach(a -> assertTrue(aUris.contains(a.getUri())));
    }

    /**
     * Bug #33
     */
    @Test
    public void mergeWithUpdatedPropertyValueRemovesOriginalAssertion() {
        this.em = getEntityManager("mergeWithUpdatedPropertyValueRemovesOriginalAssertion", false);
        persist(entityH, entityA);

        em.getTransaction().begin();
        entityH.setOwlClassA(entityA2);
        em.merge(entityH);
        em.getTransaction().commit();

        final OWLClassH result = em.find(OWLClassH.class, entityH.getUri());
        assertEquals(entityA2.getUri(), result.getOwlClassA().getUri());
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    /**
     * Bug #33, but for plural attributes
     */
    @Test
    public void mergeWithUpdatedPropertyValueRemovesOriginalAssertionsFromPluralAttribute() {
        this.em = getEntityManager("mergeWithUpdatedPropertyValueRemovesOriginalAssertionsFromPluralAttribute", false);
        persist(entityJ);

        assertEquals(2, entityJ.getOwlClassA().size());
        final OWLClassA newA = new OWLClassA(Generators.generateUri());
        entityJ.setOwlClassA(Collections.singleton(newA));
        em.getTransaction().begin();
        em.merge(entityJ);
        em.getTransaction().commit();

        final OWLClassJ result = em.find(OWLClassJ.class, entityJ.getUri());
        assertEquals(1, result.getOwlClassA().size());
        assertEquals(newA.getUri(), result.getOwlClassA().iterator().next().getUri());
    }

    @Test
    public void updateSupportsJava8Instant() {
        this.em = getEntityManager("updateSupportsJava8Instant", true);
        final OWLClassX entityX = new OWLClassX();
        entityX.setInstant(Instant.now());
        persist(entityX);

        final Instant newInstant = Instant.ofEpochMilli(System.currentTimeMillis() + 10000);
        em.getTransaction().begin();
        final OWLClassX toUpdate = em.find(OWLClassX.class, entityX.getUri());
        toUpdate.setInstant(newInstant);
        em.getTransaction().commit();

        final OWLClassX cachedResult = em.find(OWLClassX.class, entityX.getUri());
        assertEquals(newInstant, cachedResult.getInstant());
        em.getEntityManagerFactory().getCache().evict(OWLClassX.class);
        final OWLClassX result = em.find(OWLClassX.class, entityX.getUri());
        assertEquals(newInstant, result.getInstant());
    }

    @Test
    public void updateSupportsPluralAnnotationProperty() {
        this.em = getEntityManager("updateSupportsPluralAnnotationProperty", true);
        final Set<String> annotations = IntStream.range(0, 5).mapToObj(i -> "Source" + i).collect(Collectors.toSet());
        entityN.setPluralAnnotationProperty(annotations);
        persist(entityN);

        final String added = "Added Source";
        final String removed = annotations.iterator().next();
        annotations.remove(removed);
        annotations.add(added);
        entityN.getPluralAnnotationProperty().remove(removed);
        entityN.getPluralAnnotationProperty().add(added);
        em.getTransaction().begin();
        final OWLClassN merged = em.merge(entityN);
        assertEquals(annotations, merged.getPluralAnnotationProperty());
        em.getTransaction().commit();

        final OWLClassN result = em.find(OWLClassN.class, entityN.getId());
        assertEquals(annotations, result.getPluralAnnotationProperty());
    }
}
