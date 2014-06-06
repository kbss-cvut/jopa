package cz.cvut.kbss.jopa.test.integration.runners;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.OWLClassJ;
import cz.cvut.kbss.jopa.test.utils.Generators;

public class UpdateOperationsRunner {

	private OWLClassA entityA;
	private OWLClassA entityA2;

	private OWLClassB entityB;
	private OWLClassC entityC;
	private OWLClassD entityD;
	// Generated IRI
	private OWLClassE entityE;
	// Lazy reference to OWLClassA
	private OWLClassI entityI;
	// Two relationships
	private OWLClassG entityG;
	private OWLClassH entityH;
	private OWLClassJ entityJ;

	public UpdateOperationsRunner() {
		init();
	}

	private void init() {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		entityA.setTypes(types);
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityC = new OWLClassC();
		entityC.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityI"));
		entityI.setOwlClassA(entityA);
		entityH = new OWLClassH();
		entityH.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityH"));
		entityH.setOwlClassA(entityA);
		entityG = new OWLClassG();
		entityG.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
		entityG.setOwlClassH(entityH);

		entityA2 = new OWLClassA();
		entityA2.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA2"));
		entityJ = new OWLClassJ();
		entityJ.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityJ"));
		final Set<OWLClassA> set = new HashSet<>(2);
		set.add(entityA);
		set.add(entityA2);
		entityJ.setOwlClassA(set);
	}

	public void initBeforeTest() {
		entityB.setProperties(null);
		entityC.setSimpleList(null);
		entityC.setReferencedList(null);
		entityE.setUri(null);
	}

	public void mergeList(EntityManager em, URI ctx) {
		final EntityDescriptor jDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		em.getTransaction().begin();
		em.persist(entityJ, jDescriptor);
		em.getTransaction().commit();
		em.clear();

		for (final OWLClassA a : entityJ.getOwlClassA()) {
			a.setStringAttribute("NEWVALUE");
		}

		em.getTransaction().begin();
		OWLClassJ merged = em.merge(entityJ, jDescriptor);

		for (final OWLClassA a : merged.getOwlClassA()) {
			assertEquals(a.getStringAttribute(), "NEWVALUE");
		}
		em.getTransaction().commit();
	}

	public void updateDataPropertyKeepLazyEmpty(EntityManager em, URI ctx) throws Exception {
		final EntityDescriptor bDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityB.setProperties(Generators.createProperties());
		em.getTransaction().begin();
		em.persist(entityB, bDescriptor);
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassB b = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
		assertNotNull(b);
		final Field propsField = OWLClassB.getPropertiesField();
		propsField.setAccessible(true);
		assertNull(propsField.get(b));
		final String newString = "NewString";
		b.setStringAttribute(newString);
		em.getTransaction().commit();

		final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
		assertNotNull(res);
		assertEquals(newString, res.getStringAttribute());
		assertNotNull(res.getProperties());
		assertEquals(entityB.getProperties(), res.getProperties());
	}

	public void updateDataPropertySetNull(EntityManager em, URI ctx) {
		final EntityDescriptor aDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(a);
		assertNotNull(a.getStringAttribute());
		a.setStringAttribute(null);
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(res);
		assertNull(res.getStringAttribute());
		assertEquals(entityA.getTypes(), res.getTypes());
	}

	public void updateReference(EntityManager em, URI ctx) {
		final EntityDescriptor dDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		final EntityDescriptor iDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		em.getTransaction().begin();
		em.persist(entityD, dDescriptor);
		// em.persist(entityA, ctx);
		em.persist(entityI, iDescriptor);
		em.getTransaction().commit();

		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newEntityA"));
		newA.setStringAttribute("newA");
		em.getTransaction().begin();
		final OWLClassD d = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
		assertNotNull(d.getOwlClassA());
		d.setOwlClassA(newA);
		final OWLClassI i = em.find(OWLClassI.class, entityI.getUri(), iDescriptor);
		assertNotNull(i.getOwlClassA());
		i.setOwlClassA(newA);
		em.persist(newA, iDescriptor);
		em.getTransaction().commit();

		final OWLClassA resA1 = em.find(OWLClassA.class, newA.getUri(), dDescriptor);
		assertNotNull(resA1);
		final OWLClassD resD = em.find(OWLClassD.class, d.getUri(), dDescriptor);
		assertSame(resD.getOwlClassA(), resA1);
		assertNotNull(em.find(OWLClassA.class, entityA.getUri(), iDescriptor));
		final OWLClassI resI = em.find(OWLClassI.class, i.getUri(), iDescriptor);
		assertEquals(newA.getUri(), resI.getOwlClassA().getUri());
		assertNotNull(em.find(OWLClassA.class, entityA.getUri(), dDescriptor));
		assertEquals(resA1, resI.getOwlClassA());
	}

	public void mergeDetachedWithChanges(EntityManager em, URI ctx) {
		final EntityDescriptor aDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertTrue(em.contains(a));
		em.detach(a);
		assertFalse(em.contains(a));
		final String newType = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#AddedType";
		a.getTypes().add(newType);
		em.getTransaction().begin();
		final OWLClassA merged = em.merge(a, aDescriptor);
		assertTrue(merged.getTypes().contains(newType));
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, a.getUri(), aDescriptor);
		assertEquals(a.getTypes().size(), res.getTypes().size());
		assertTrue(res.getTypes().containsAll(a.getTypes()));
	}

	public void mergeDetachedCascade(EntityManager em, URI ctx) {
		final EntityDescriptor hDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		em.getTransaction().begin();
		em.persist(entityH, hDescriptor);
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();

		final OWLClassH h = em.find(OWLClassH.class, entityH.getUri(), hDescriptor);
		assertNotNull(h.getOwlClassA());
		em.detach(h);
		assertFalse(em.contains(h));
		assertFalse(em.contains(h.getOwlClassA()));
		final String newStr = "newStringAttribute";
		h.getOwlClassA().setStringAttribute(newStr);
		em.getTransaction().begin();
		final OWLClassH merged = em.merge(h, hDescriptor);
		assertEquals(newStr, merged.getOwlClassA().getStringAttribute());
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), hDescriptor);
		assertNotNull(res);
		assertEquals(newStr, res.getStringAttribute());
	}

	public void removeFromSimpleList(EntityManager em, URI ctx) {
		final EntityDescriptor cDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityC.setSimpleList(Generators.createSimpleList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		em.getTransaction().begin();
		final OWLClassA a = c.getSimpleList().get(1);
		c.getSimpleList().remove(a);
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), cDescriptor);
		assertNotNull(resA);
		final OWLClassC resC = em.find(OWLClassC.class, c.getUri(), cDescriptor);
		assertEquals(c.getSimpleList().size(), resC.getSimpleList().size());
		assertEquals(entityC.getSimpleList().size() - 1, resC.getSimpleList().size());
		for (OWLClassA aa : resC.getSimpleList()) {
			assertFalse(resA.getUri().equals(aa.getUri()));
		}
	}

	public void addToSimpleList(EntityManager em, URI ctx) {
		final EntityDescriptor cDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityC.setSimpleList(Generators.createSimpleList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, cDescriptor);
		}
		final EntityDescriptor aDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(a);
		assertFalse(c.getSimpleList().contains(a));
		c.getSimpleList().add(a);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertEquals(c.getSimpleList().size(), resC.getSimpleList().size());
		assertEquals(entityC.getSimpleList().size() + 1, resC.getSimpleList().size());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(resA);
		assertTrue(resC.getSimpleList().contains(resA));
	}

	public void clearSimpleList(EntityManager em, URI ctx) {
		final EntityDescriptor cDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityC.setSimpleList(Generators.createSimpleList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		assertFalse(c.getSimpleList().isEmpty());
		em.getTransaction().begin();
		c.getSimpleList().clear();
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(resC);
		assertTrue(resC.getSimpleList().isEmpty());
		for (OWLClassA a : entityC.getSimpleList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri(), cDescriptor));
		}
	}

	public void replaceSimpleList(EntityManager em, URI ctx) {
		final EntityDescriptor cDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityC.setSimpleList(Generators.createSimpleList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		final List<OWLClassA> newList = new ArrayList<>(1);
		newList.add(entityA);
		em.getTransaction().begin();
		em.persist(entityA, cDescriptor);
		c.setSimpleList(newList);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(resC);
		assertEquals(newList.size(), resC.getSimpleList().size());
		boolean found = false;
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
			assertNotNull(em.find(OWLClassA.class, a.getUri(), cDescriptor));
		}
	}

	public void removeFromReferencedList(EntityManager em, URI ctx) {
		final EntityDescriptor cDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityC.setReferencedList(Generators.createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		em.getTransaction().begin();
		final OWLClassA a = c.getReferencedList().get(3);
		c.getReferencedList().remove(a);
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), cDescriptor);
		assertNotNull(resA);
		final OWLClassC resC = em.find(OWLClassC.class, c.getUri(), cDescriptor);
		assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
		assertEquals(entityC.getReferencedList().size() - 1, resC.getReferencedList().size());
		for (OWLClassA aa : resC.getReferencedList()) {
			assertFalse(resA.getUri().equals(aa.getUri()));
		}
	}

	public void addToReferencedList(EntityManager em, URI ctx) {
		final EntityDescriptor cDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityC.setReferencedList(Generators.createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		em.persist(entityA, cDescriptor);
		c.getReferencedList().add(entityA);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
		assertEquals(entityC.getReferencedList().size() + 1, resC.getReferencedList().size());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), cDescriptor);
		assertNotNull(resA);
		assertTrue(resC.getReferencedList().contains(resA));
	}

	public void clearReferencedList(EntityManager em, URI ctx) {
		final EntityDescriptor cDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityC.setReferencedList(Generators.createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		assertFalse(c.getReferencedList().isEmpty());
		em.getTransaction().begin();
		c.setReferencedList(null);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(resC);
		assertNull(resC.getReferencedList());
		for (OWLClassA a : entityC.getReferencedList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri(), cDescriptor));
		}
	}

	public void replaceReferencedList(EntityManager em, URI ctx) {
		final EntityDescriptor cDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityC.setReferencedList(Generators.createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		final List<OWLClassA> newList = new ArrayList<>(1);
		newList.add(entityA);
		em.getTransaction().begin();
		em.persist(entityA, cDescriptor);
		c.setReferencedList(newList);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(resC);
		assertEquals(newList.size(), resC.getReferencedList().size());
		boolean found = false;
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
			assertNotNull(em.find(OWLClassA.class, a.getUri(), cDescriptor));
		}
	}

	public void addNewToProperties(EntityManager em, URI ctx) {
		final EntityDescriptor bDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityB.setProperties(Generators.createProperties());
		final Map<String, Set<String>> expected = new HashMap<>(entityB.getProperties().size() + 3);
		expected.putAll(entityB.getProperties());
		em.getTransaction().begin();
		em.persist(entityB, bDescriptor);
		em.getTransaction().commit();
		em.clear();

		final OWLClassB b = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
		assertNotNull(b);
		assertEquals(expected.size(), b.getProperties().size());
		em.getTransaction().begin();
		b.getProperties().put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyFour",
				Collections.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/Stroustrup"));
		expected.putAll(b.getProperties());
		em.getTransaction().commit();

		final OWLClassB res = em.find(OWLClassB.class, b.getUri(), bDescriptor);
		assertNotNull(res);
		assertEquals(expected.size(), res.getProperties().size());
		for (Entry<String, Set<String>> e : expected.entrySet()) {
			assertTrue(res.getProperties().containsKey(e.getKey()));
			final Set<String> s = e.getValue();
			assertEquals(1, s.size());
			final Set<String> resS = res.getProperties().get(e.getKey());
			assertEquals(s.size(), resS.size());
			assertTrue(resS.contains(s.iterator().next()));
		}
	}

	public void addPropertyValue(EntityManager em, URI ctx) {
		final EntityDescriptor bDescriptor = EntityDescriptor.createWithEntityContext(ctx);
		entityB.setProperties(Generators.createProperties());
		final Map<String, Set<String>> expected = new HashMap<>(entityB.getProperties().size() + 3);
		final String prop = entityB.getProperties().keySet().iterator().next();
		expected.putAll(entityB.getProperties());
		em.getTransaction().begin();
		em.persist(entityB, bDescriptor);
		em.getTransaction().commit();
		em.clear();

		final OWLClassB b = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
		assertNotNull(b);
		assertEquals(expected.size(), b.getProperties().size());
		em.getTransaction().begin();
		b.getProperties().get(prop).add("http://krizik.felk.cvut.cz/ontologies/jopa/Stroustrup");
		expected.putAll(b.getProperties());
		em.getTransaction().commit();

		final OWLClassB res = em.find(OWLClassB.class, b.getUri(), bDescriptor);
		assertNotNull(res);
		assertEquals(expected.size(), res.getProperties().size());
		for (Entry<String, Set<String>> e : expected.entrySet()) {
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
}
