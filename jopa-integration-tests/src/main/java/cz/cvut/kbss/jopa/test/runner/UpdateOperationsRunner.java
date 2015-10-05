package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public abstract class UpdateOperationsRunner extends BaseRunner {

    private OWLClassA entityA2;
    private OWLClassF entityF;
    private OWLClassJ entityJ;
    private OWLClassO entityO;

    public UpdateOperationsRunner(Logger logger) {
        super(logger);
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
        logger.config(
                "Test: update data property. Leaves lazily loaded field empty and checks that after commit the field's value hasn't changed.");
        this.em = getEntityManager("UpdateDataProperty", false);
        entityB.setProperties(Generators.createProperties());
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();

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
        logger.config("Test: update data property. Set it to null.");
        this.em = getEntityManager("UpdateDataPropertyToNull", true);
        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();

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
        logger.config("Test: update reference to entity.");
        this.em = getEntityManager("UpdateReference", true);
        em.getTransaction().begin();
        em.persist(entityD);
        // em.persist(entityA, ctx);
        em.persist(entityI);
        em.getTransaction().commit();

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
    public void testMergeSet() throws Exception {
        logger.config("Test: merge set property.");
        this.em = getEntityManager("MergeSet", false);
        em.getTransaction().begin();
        em.persist(entityJ);
        em.getTransaction().commit();
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
        logger.config("Test: merge detached entity with changes.");
        this.em = getEntityManager("UpdateDetached", true);
        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();

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
        logger.config("Test: merge detached with cascade.");
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
        logger.config("Test: merge detached with object property change.");
        this.em = getEntityManager("UpdateDetachedWithOPChange", true);
        em.getTransaction().begin();
        em.persist(entityD);
        em.persist(entityA);
        em.getTransaction().commit();

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
        logger.config("Test: remove entity from simple list. (But keep it in the ontology.)");
        this.em = getEntityManager("UpdateRemoveFromSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.getTransaction().commit();

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
            assertFalse(resA.getUri().equals(aa.getUri()));
        }
    }

    @Test
    public void testAddToSimpleList() {
        logger.config("Test: add entity to simple list.");
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
        logger.config("Test: clear a simple list (but keep the entities in ontology).");
        this.em = getEntityManager("UpdateClearSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.getTransaction().commit();

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
        logger.config("Test: replace simple list with a new one.");
        this.em = getEntityManager("UpdateReplaceSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.getTransaction().commit();

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
        logger.config("Test: remove entity from referenced list. (But keep it in the ontology.");
        this.em = getEntityManager("UpdateRemoveFromReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        final OWLClassA a = c.getReferencedList().get(3);
        c.getReferencedList().remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNotNull(resA);
        final OWLClassC resC = em.find(OWLClassC.class, c.getUri());
        assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
        assertEquals(entityC.getReferencedList().size() - 1, resC.getReferencedList().size());
        for (OWLClassA aa : resC.getReferencedList()) {
            assertFalse(resA.getUri().equals(aa.getUri()));
        }
    }

    @Test
    public void testAddToReferencedList() {
        logger.config("Test: add entity to Referenced list.");
        this.em = getEntityManager("UpdateAddToReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

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
        logger.config("Test: clear referenced list (but keep the entities in ontology).");
        this.em = getEntityManager("UpdateClearReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

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
        logger.config("Test: replace referenced list with a new one.");
        this.em = getEntityManager("UpdateReplaceReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

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
        logger.config("Test: add a new property value to entity's properties.");
        this.em = getEntityManager("UpdateAddNewToProperties", false);
        entityB.setProperties(Generators.createProperties());
        final Map<String, Set<String>> expected = new HashMap<>(entityB.getProperties().size() + 3);
        expected.putAll(entityB.getProperties());
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();
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
            assertEquals(1, s.size());
            final Set<String> resS = res.getProperties().get(e.getKey());
            assertEquals(s.size(), resS.size());
            assertTrue(resS.contains(s.iterator().next()));
        }
    }

    @Test
    public void testAddPropertyValue() {
        logger.config("Test: add another value to an existing property.");
        this.em = getEntityManager("UpdateAddPropertyValue", false);
        entityB.setProperties(Generators.createProperties());
        final Map<String, Set<String>> expected = new HashMap<>(entityB.getProperties().size() + 3);
        final String prop = entityB.getProperties().keySet().iterator().next();
        expected.putAll(entityB.getProperties());
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();
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
        logger.config("Test: add another value to an existing property.");
        this.em = getEntityManager("UpdateAddPropertyValueDetached", false);
        entityB.setProperties(Generators.createProperties());
        final String prop = entityB.getProperties().keySet().iterator().next();
        final String newPropertyValue = "http://krizik.felk.cvut.cz/ontologies/jopa#newPropertyValue";
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();
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

    @Test(expected = OWLInferredAttributeModifiedException.class)
    public void testModifyInferredAttribute() {
        logger.config("Test: modify an inferred attribute.");
        this.em = getEntityManager("ModifyInferredAttribute", false);
        em.getTransaction().begin();
        em.persist(entityF);
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassF f = em.find(OWLClassF.class, entityF.getUri());
        assertNotNull(f);
        f.setSecondStringAttribute("otherValue");
        em.getTransaction().commit();
    }

    @Test
    public void testModifyAttributesOfBasicTypes() {
        logger.config("Test: modify attributes of basic Java types (Integer, Boolean etc.).");
        this.em = getEntityManager("ModifyBasicTypeAttributes", false);
        em.getTransaction().begin();
        em.persist(entityM);
        em.getTransaction().commit();

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
        logger.config("Test: modify enum attribute.");
        this.em = getEntityManager("ModifyEnumAttribute", false);
        assertNotNull(entityM.getEnumAttribute());
        em.getTransaction().begin();
        em.persist(entityM);
        em.getTransaction().commit();

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
        logger.config("Test: cascade merging of an object that has been just persisted.");
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
}
