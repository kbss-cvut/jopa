package cz.cvut.kbss.owlpersistence.owlapi.transactionsUnitTests;

import static org.junit.Assert.*;

import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.owlpersistence.accessors.OntologyAccessor;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassA;
import cz.cvut.kbss.owlpersistence.sessions.CacheManagerImpl;
import cz.cvut.kbss.owlpersistence.sessions.ServerSession;
import cz.cvut.kbss.owlpersistence.sessions.UnitOfWork;

public class CacheManagerTest {
	
	private SessionStub session;
	private OWLClassA testObject;
	private CacheManagerImpl mngr;

	@Before
	public void setUp() throws Exception {
		final AccessorStub accessor = new AccessorStub();
		this.session = new SessionStub(accessor);
		final URI pk = URI.create("http://testEntity");
		this.testObject = new OWLClassA();
		this.testObject.setUri(pk);
		this.testObject.setStringAttribute("testAttribute");
		this.mngr = new CacheManagerImpl(session);
	}

	@Test
	public void testAddObjectIntoCache() {
		this.mngr.addObjectIntoCache(testObject);
		assertEquals(testObject, mngr.getLiveObjectCache().get(testObject));
		Object uri = mngr.getIRIOfObject(testObject);
		assertEquals(IRI.create(testObject.getUri()), uri);
	}
	
	@Test
	public void testAddObjectWithDuplicateIRI() {
		this.mngr.addObjectIntoCache(testObject);
		final OWLClassA duplicate = new OWLClassA();
		duplicate.setUri(testObject.getUri());
		try {
			this.mngr.addObjectIntoCache(duplicate);
			fail();
		} catch (OWLPersistenceException e) {
			System.out.println("Exception raised! Right.");
			e.printStackTrace();
		}
	}

	@Test
	public void testAddObjectWithIRIIntoCache() {
		this.mngr.addObjectIntoCache(testObject, testObject.getUri());
		assertFalse(mngr.getLiveObjectCache().isEmpty());
	}

	@Test
	public void testAddObjectsIntoCache() {
		final OWLClassA tmp = new OWLClassA();
		final URI pk = URI.create("http://testURI");
		tmp.setUri(pk);
		final Map<URI, OWLClassA> map = new HashMap<URI, OWLClassA>();
		map.put(testObject.getUri(), testObject);
		map.put(pk, tmp);
		this.mngr.addObjectsIntoCache(map);
		OWLClassA res = (OWLClassA) mngr.getLiveObjectCache().get(testObject);
		assertEquals(testObject.getStringAttribute(), res.getStringAttribute());
	}

	@Test
	public void testContainsObject() {
		this.mngr.addObjectIntoCache(testObject);
		assertTrue(mngr.containsObject(testObject));
	}

	@Test
	public void testGetObject() {
		this.mngr.addObjectIntoCache(testObject);
		assertEquals(testObject, mngr.getObject(testObject));
	}
	
	@Test
	public void testGetObjectByValue() {
		this.mngr.addObjectIntoCache(testObject);
		assertEquals(testObject, mngr.getObjectByValue(testObject));
	}

	@Test
	public void testRemoveObjectFromCache() {
		this.mngr.addObjectIntoCache(testObject);
		this.mngr.removeObjectFromCache(testObject);
		assertTrue(mngr.getLiveObjectCache().isEmpty());
	}

	@Test
	public void testContainsObjectByIRI() {
		this.mngr.addObjectIntoCache(testObject);
		final IRI iri = IRI.create(testObject.getUri());
		assertTrue(mngr.containsObjectByIRI(iri));
	}

	@Test
	public void testGetObjectByIRI() {
		this.mngr.addObjectIntoCache(testObject);
		final IRI iri = IRI.create(testObject.getUri());
		Object res = mngr.getObjectByIRI(iri);
		assertEquals(testObject, res);
	}

	@Test
	public void testGetIRIOfObject() {
		this.mngr.addObjectIntoCache(testObject);
		final IRI iri = IRI.create(testObject.getUri());
		Object res = mngr.getIRIOfObject(testObject);
		assertEquals(iri, res);
	}
	
	@Test
	public void testReleaseCache() {
		this.mngr.addObjectIntoCache(testObject);
		this.mngr.releaseCache();
		assertNull(mngr.getObject(testObject));
	}

	@Test
	public void testRemoveObjectFromCacheByIRI() {
		this.mngr.addObjectIntoCache(testObject);
		final URI pk = URI.create("http://testURI");
		final OWLClassA tmp = new OWLClassA();
		tmp.setUri(pk);
		this.mngr.addObjectIntoCache(tmp, pk);
		this.mngr.removeObjectFromCacheByIRI(IRI.create(testObject.getUri()));
		assertNull(mngr.getObjectByIRI(IRI.create(testObject.getUri())));
	}
	
	private class SessionStub extends ServerSession {
		
		public SessionStub(AccessorStub accessor) {
			this.accessor = accessor;
		}
		
		public OntologyAccessor getOntologyAccessor() {
			return this.accessor;
		}
	}
	
	private class AccessorStub implements OntologyAccessor {

		public void persistEntity(Object entity, UnitOfWork uow) {
		}

		public void removeEntity(Object entity) {
		}

		public <T> T readEntity(Class<T> cls, Object uri) {
			return null;
		}

		public void writeChanges(List<OWLOntologyChange> changes) {
		}

		public void writeChange(OWLOntologyChange change) {
			
		}

		public void saveWorkingOntology() {
		}

		public boolean isInOntologySignature(IRI uri, boolean searchImports) {
			return false;
		}

		public OWLNamedIndividual getOWLNamedIndividual(IRI identifier) {
			return null;
		}

		/**
		 * We need only this method
		 */
		public IRI getIdentifier(Object object) {
			OWLClassA tmp = (OWLClassA) object;
			return IRI.create(tmp.getUri());
		}

		public void persistExistingEntity(Object entity, UnitOfWork uow) {
		}
		
	}

}
