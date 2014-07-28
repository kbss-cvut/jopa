package cz.cvut.kbss.ontodriver.test.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.ontodriver.Connection;

final class SesameSingleContextTests {

	private static final Descriptor DEFAULT_DESCRIPTOR = new EntityDescriptor();

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
		OWLClassB res = c.find(OWLClassB.class, URI.create("http://someUnknownB"),
				DEFAULT_DESCRIPTOR);
		assertNull(res);
	}

	public void testPersistSimple() throws Exception {
		LOG.config("Test: persist a simple entity.");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB, DEFAULT_DESCRIPTOR);
		c.commit();
		assertTrue(c.contains(entityB.getUri(), null));
		final OWLClassB res = c.find(OWLClassB.class, entityB.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(res);
		assertEquals(entityB.getUri(), res.getUri());
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
	}

	public void testPersistWithTypes() throws Exception {
		LOG.config("Test: persist entity with types.");
		c.persist(entityA.getUri(), entityA, DEFAULT_DESCRIPTOR);
		// Let auto commit do its work
		assertTrue(c.contains(entityA.getUri(), null));
		final OWLClassA res = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
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
		c.persist(null, entityE, DEFAULT_DESCRIPTOR);
		assertNotNull(entityE.getUri());
		c.commit();
		assertTrue(c.contains(entityE.getUri(), null));
		final OWLClassE res = c.find(OWLClassE.class, entityE.getUri(), DEFAULT_DESCRIPTOR);
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
			c.persist(null, e, DEFAULT_DESCRIPTOR);
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
		c.persist(entityD.getUri(), entityD, DEFAULT_DESCRIPTOR);
		c.persist(entityA.getUri(), entityA, DEFAULT_DESCRIPTOR);
		c.commit();
		final OWLClassD resD = c.find(OWLClassD.class, entityD.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resD);
		assertEquals(entityD.getUri(), resD.getUri());
		assertNotNull(resD.getOwlClassA());
		assertEquals(entityA.getUri(), resD.getOwlClassA().getUri());
		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resA);
	}

	public void testPersistTwice() throws Exception {
		LOG.config("Test: persist entity twice.");
		c.persist(entityB.getUri(), entityB, DEFAULT_DESCRIPTOR);
		assertTrue(c.contains(entityB.getUri(), null));
		c.persist(entityB.getUri(), entityB, DEFAULT_DESCRIPTOR);
		fail("This line should not have been reached.");
	}

	public void testPersistTwiceInTransaction() throws Exception {
		LOG.config("Test: persist entity twice in one transaction.");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB, DEFAULT_DESCRIPTOR);
		assertTrue(c.contains(entityB.getUri(), null));
		c.persist(entityB.getUri(), entityB, DEFAULT_DESCRIPTOR);
		fail("This line should not have been reached.");
	}

	public void testUpdateDataPropertyValue() throws Exception {
		LOG.config("Test: update data property value of an entity.");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB, DEFAULT_DESCRIPTOR);
		c.commit();
		final OWLClassB b = c.find(OWLClassB.class, entityB.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(b);
		final String newString = "newStringAttributeValue";
		b.setStringAttribute(newString);
		final Field strField = OWLClassB.getStrAttField();
		c.merge(b, strField, DEFAULT_DESCRIPTOR);
		c.commit();
		final OWLClassB res = c.find(OWLClassB.class, entityB.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(res);
		assertEquals(newString, res.getStringAttribute());
	}

	public void testUpdateTypes() throws Exception {
		LOG.config("Test: update types of an entity.");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA, DEFAULT_DESCRIPTOR);
		c.commit();
		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(a);
		final String newType = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#newTypeA";
		final String toRemove = entityA.getTypes().iterator().next();
		a.getTypes().remove(toRemove);
		a.getTypes().add(newType);
		final Field typesField = OWLClassA.getTypesField();
		c.merge(a, typesField, DEFAULT_DESCRIPTOR);
		c.commit();

		final OWLClassA res = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(res);
		assertFalse(res.getTypes().isEmpty());
		assertTrue(res.getTypes().contains(newType));
		assertFalse(res.getTypes().contains(toRemove));
	}

	public void testUpdateObjectProperty() throws Exception {
		LOG.config("Test: update object property value.");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA, DEFAULT_DESCRIPTOR);
		c.persist(entityD.getUri(), entityD, DEFAULT_DESCRIPTOR);
		c.commit();
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newEntityA"));
		newA.setStringAttribute("newAsStringAttribute");
		final OWLClassD d = c.find(OWLClassD.class, entityD.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(d);
		d.setOwlClassA(newA);
		final Field aField = OWLClassD.getOwlClassAField();
		c.merge(d, aField, DEFAULT_DESCRIPTOR);
		c.persist(newA.getUri(), newA, DEFAULT_DESCRIPTOR);
		c.commit();

		final OWLClassD resD = c.find(OWLClassD.class, d.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		assertEquals(newA.getUri(), resD.getOwlClassA().getUri());
		final OWLClassA resA = c.find(OWLClassA.class, newA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resA);
		assertEquals(resD.getOwlClassA().getUri(), resA.getUri());
		final OWLClassA resAOld = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resAOld);
	}

	public void testUpdateObjectPropertyToNull() throws Exception {
		LOG.config("Test: update object property value. Set it to null.");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA, DEFAULT_DESCRIPTOR);
		c.persist(entityD.getUri(), entityD, DEFAULT_DESCRIPTOR);
		c.commit();
		final OWLClassD d = c.find(OWLClassD.class, entityD.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(d);
		d.setOwlClassA(null);
		final Field aField = OWLClassD.getOwlClassAField();
		c.merge(d, aField, DEFAULT_DESCRIPTOR);
		c.commit();
		final OWLClassD resD = c.find(OWLClassD.class, d.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resD);
		assertNull(resD.getOwlClassA());
		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resA);
	}

	public void updateDataPropertyToNull() throws Exception {
		LOG.config("Test: update data property value. Set it to null.");
		c.persist(entityA.getUri(), entityA, DEFAULT_DESCRIPTOR);

		c.setAutoCommit(false);
		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(a);
		assertNotNull(a.getStringAttribute());
		a.setStringAttribute(null);
		final Field strField = OWLClassA.getStrAttField();
		c.merge(a, strField, DEFAULT_DESCRIPTOR);
		c.commit();

		final OWLClassA res = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(res);
		assertNull(res.getStringAttribute());
	}

	public void testRemove() throws Exception {
		LOG.config("Test: remove entity.");
		c.persist(entityA.getUri(), entityA, DEFAULT_DESCRIPTOR);
		// auto commit
		c.persist(entityB.getUri(), entityB, DEFAULT_DESCRIPTOR);
		// auto commit
		c.persist(entityE.getUri(), entityE, DEFAULT_DESCRIPTOR);
		// auto commit
		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(a);
		c.remove(a.getUri(), DEFAULT_DESCRIPTOR); // auto commit
		assertFalse(c.contains(a.getUri(), null));
		assertNull(c.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR));
		final OWLClassE e = c.find(OWLClassE.class, entityE.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(e);
		c.remove(e.getUri(), DEFAULT_DESCRIPTOR); // auto commit
		assertFalse(c.contains(e.getUri(), null));
		assertNull(c.find(OWLClassE.class, e.getUri(), DEFAULT_DESCRIPTOR));
		assertTrue(c.contains(entityB.getUri(), null));
	}
}
