package cz.cvut.kbss.ontodriver.test.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.ontodriver.Connection;

final class SesameSingleContextTests {

	static final String A_STRING_ATT = "entityAStringAttribute";

	OWLClassA entityA;
	OWLClassB entityB;
	OWLClassD entityD;
	// ID generated
	OWLClassE entityE;

	private final Logger LOG;
	private Connection c;

	SesameSingleContextTests(Logger logger) {
		this.LOG = logger;

		this.entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute(A_STRING_ATT);
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLTypeForA");
		entityA.setTypes(types);
		this.entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		this.entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		this.entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
	}

	void setConnection(Connection c) {
		this.c = c;
	}

	public void testAcquireConnection() throws Exception {
		LOG.config("Test: just acquire connection and make sure it is open.");
		assertNotNull(c);
		assertTrue(c.isOpen());
		// Make the connection initialize the storage module by asking for some
		// entity
		OWLClassB res = c.find(OWLClassB.class, URI.create("http://someUnknownB"));
		assertNull(res);
	}

	public void testPersistSimple() throws Exception {
		LOG.config("Test: persist a simple entity.");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB);
		c.commit();
		assertTrue(c.contains(entityB.getUri()));
		final OWLClassB res = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(res);
		assertEquals(entityB.getUri(), res.getUri());
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
	}

	public void testPersistWithTypes() throws Exception {
		LOG.config("Test: persist entity with types.");
		c.persist(entityA.getUri(), entityA);
		// Let auto commit do its work
		assertTrue(c.contains(entityA.getUri()));
		final OWLClassA res = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getUri());
		assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
		assertFalse(res.getTypes().isEmpty());
		assertEquals(entityA.getTypes().size(), res.getTypes().size());
		assertTrue(entityA.getTypes().containsAll(res.getTypes()));
	}

	public void testPersistWithIdGenerated() throws Exception {
		LOG.config("Test: persist with ID generation.");
		c.setAutoCommit(false);
		assertNull(entityE.getUri());
		c.persist(null, entityE);
		assertNotNull(entityE.getUri());
		c.commit();
		assertTrue(c.contains(entityE.getUri()));
		final OWLClassE res = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(res);
		assertEquals(entityE.getUri(), res.getUri());
		assertEquals(entityE.getStringAttribute(), res.getStringAttribute());
	}

	public void testPersistWithIdGeneratedMultiple() throws Exception {
		LOG.config("Test: persist multiple entities with generated id.");
		final Set<URI> generated = new HashSet<>();
		c.setAutoCommit(false);
		final int cnt = 10;
		for (int i = 0; i < cnt; i++) {
			final OWLClassE e = new OWLClassE();
			e.setStringAttribute("stringNo" + i);
			assertNull(e.getUri());
			c.persist(null, e);
			assertNotNull(e.getUri());
			assertFalse(generated.contains(e.getUri()));
			generated.add(e.getUri());
			if (i % 5 == 0 && i > 1) {
				c.commit();
			}
		}
		assertEquals(cnt, generated.size());
	}

	public void testPersistWithObjectProperty() throws Exception {
		LOG.config("Test: persist with object property, i. e. reference to another entity.");
		c.setAutoCommit(false);
		c.persist(entityD.getUri(), entityD);
		c.persist(entityA.getUri(), entityA);
		c.commit();
		final OWLClassD resD = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		assertEquals(entityD.getUri(), resD.getUri());
		assertNotNull(resD.getOwlClassA());
		assertEquals(entityA.getUri(), resD.getOwlClassA().getUri());
		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
	}

	public void testPersistTwice() throws Exception {
		LOG.config("Test: persist entity twice.");
		c.persist(entityB.getUri(), entityB);
		assertTrue(c.contains(entityB.getUri()));
		c.persist(entityB.getUri(), entityB);
		fail("This line should not have been reached.");
	}

	public void testPersistTwiceInTransaction() throws Exception {
		LOG.config("Test: persist entity twice in one transaction.");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB);
		assertTrue(c.contains(entityB.getUri()));
		c.persist(entityB.getUri(), entityB);
		fail("This line should not have been reached.");
	}

	public void testUpdateDataPropertyValue() throws Exception {
		LOG.config("Test: update data property value of an entity.");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB);
		c.commit();
		final OWLClassB b = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(b);
		final String newString = "newStringAttributeValue";
		b.setStringAttribute(newString);
		c.merge(entityB.getUri(), b);
		c.commit();
		final OWLClassB res = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(res);
		assertEquals(newString, res.getStringAttribute());
	}

	public void testUpdateTypes() throws Exception {
		LOG.config("Test: update types of an entity.");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.commit();
		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		final String newType = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#newTypeA";
		final String toRemove = entityA.getTypes().iterator().next();
		a.getTypes().remove(toRemove);
		a.getTypes().add(newType);
		c.merge(a.getUri(), a);
		c.commit();

		final OWLClassA res = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(res);
		assertFalse(res.getTypes().isEmpty());
		assertTrue(res.getTypes().contains(newType));
		assertFalse(res.getTypes().contains(toRemove));
	}

	public void testUpdateObjectProperty() throws Exception {
		LOG.config("Test: update object property value.");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newEntityA"));
		newA.setStringAttribute("newAsStringAttribute");
		final OWLClassD d = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(d);
		d.setOwlClassA(newA);
		c.merge(d.getUri(), d);
		c.persist(newA.getUri(), newA);
		c.commit();

		final OWLClassD resD = c.find(OWLClassD.class, d.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		assertEquals(newA.getUri(), resD.getOwlClassA().getUri());
		final OWLClassA resA = c.find(OWLClassA.class, newA.getUri());
		assertNotNull(resA);
		assertEquals(resD.getOwlClassA().getUri(), resA.getUri());
		final OWLClassA resAOld = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resAOld);
	}

	public void testUpdateObjectPropertyToNull() throws Exception {
		LOG.config("Test: update object property value. Set it to null.");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();
		final OWLClassD d = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(d);
		d.setOwlClassA(null);
		c.merge(d.getUri(), d);
		c.commit();
		final OWLClassD resD = c.find(OWLClassD.class, d.getUri());
		assertNotNull(resD);
		assertNull(resD.getOwlClassA());
		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
	}

	public void updateDataPropertyToNull() throws Exception {
		LOG.config("Test: update data property value. Set it to null.");
		c.persist(entityA.getUri(), entityA);

		c.setAutoCommit(false);
		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		assertNotNull(a.getStringAttribute());
		a.setStringAttribute(null);
		c.merge(a.getUri(), a);
		c.commit();

		final OWLClassA res = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(res);
		assertNull(res.getStringAttribute());
	}

	public void testRemove() throws Exception {
		LOG.config("Test: remove entity.");
		c.persist(entityA.getUri(), entityA);
		// auto commit
		c.persist(entityB.getUri(), entityB);
		// auto commit
		c.persist(entityE.getUri(), entityE);
		// auto commit
		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		c.remove(a.getUri(), a); // auto commit
		assertFalse(c.contains(a.getUri()));
		assertNull(c.find(OWLClassA.class, entityA.getUri()));
		final OWLClassE e = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(e);
		c.remove(e.getUri(), e); // auto commit
		assertFalse(c.contains(e.getUri()));
		assertNull(c.find(OWLClassE.class, e.getUri()));
		assertTrue(c.contains(entityB.getUri()));
	}
}
