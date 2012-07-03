package cz.cvut.kbss.jopa.owlapi.transactionsUnitTests;

import static org.junit.Assert.*;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.accessors.OntologyAccessor;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.sessions.CloneBuilderImpl;
import cz.cvut.kbss.jopa.sessions.MergeManager;
import cz.cvut.kbss.jopa.sessions.MergeManagerImpl;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class MergeManagerTest {

	private ServerSession session;
	private UnitOfWorkImpl uow;
	private CloneBuilderStub cloneBuilder;
	private MergeManagerImpl mm;

	@Before
	public void setUp() throws Exception {
		this.session = new ServerSession();
		Field accessor = session.getClass().getDeclaredField("accessor");
		AccessorStub sor = new AccessorStub();
		accessor.setAccessible(true);
		accessor.set(session, sor);
		this.uow = (UnitOfWorkImpl) session.acquireClientSession()
				.acquireUnitOfWork();
		this.cloneBuilder = new CloneBuilderStub(uow);
		mm = new MergeManagerImpl(uow);
		// Set the stub as the clone builder
		Field builder = mm.getClass().getDeclaredField("builder");
		builder.setAccessible(true);
		builder.set(mm, cloneBuilder);
	}

	@After
	public void tearDown() throws Exception {
		session.release();
		uow.release();
	}

	@Test
	public void testMergeChangesOnObject() {
		final OWLClassB orig = new OWLClassB();
		final URI pk = URI.create("http://testObject");
		orig.setUri(pk);
		orig.setStringAttribute("ANiceAttribute");
		final OWLClassB clone = (OWLClassB) cloneBuilder.buildClone(orig);
		final ObjectChangeSetImpl chs = new ObjectChangeSetImpl(orig, clone,
				false, uow.getUowChangeSet());
		clone.setStringAttribute("AnotherStringAttribute");
		this.mm.mergeChangesOnObject(clone, chs);
		assertEquals(clone.getStringAttribute(), orig.getStringAttribute());
	}

	@Test
	public void testMergeChangesFromChangeSet() {
		final OWLClassB objOne = new OWLClassB();
		final URI pk = URI.create("http://objOne");
		objOne.setUri(pk);
		final OWLClassB objTwo = new OWLClassB();
		final URI pkTwo = URI.create("http://objTwo");
		objTwo.setUri(pkTwo);
		this.uow.getLiveObjectCache().addObjectIntoCache(objOne,
				IRI.create(objOne.getUri()));
		this.uow.getLiveObjectCache().addObjectIntoCache(objTwo,
				IRI.create(objTwo.getUri()));
		Object cloneOne = this.uow.registerExistingObject(objOne);
		Object cloneTwo = this.uow.registerExistingObject(objTwo);
		this.uow.removeObject(cloneTwo);
		((OWLClassB) cloneOne).setStringAttribute("testAtt");
		this.uow.getUowChangeSet().addDeletedObject(objTwo, cloneTwo);
		final ObjectChangeSetImpl ochs = new ObjectChangeSetImpl(objOne,
				cloneOne, false, null);
		this.uow.getUowChangeSet().addObjectChangeSet(ochs);
		this.mm.mergeChangesFromChangeSet(uow.getUowChangeSet());
		this.uow.clear();
		assertFalse(uow.contains(cloneTwo));
		assertEquals(((OWLClassB) cloneOne).getStringAttribute(),
				objOne.getStringAttribute());
	}

	@Test
	public void testMergeChangesFromChangeSetWithNew() {
		final OWLClassB objOne = new OWLClassB();
		final URI pk = URI.create("http://newOnesUri");
		objOne.setUri(pk);
		objOne.setStringAttribute("ABeautifulAttribute");
		final Object clone = cloneBuilder.buildClone(objOne);
		final ObjectChangeSetImpl ochs = new ObjectChangeSetImpl(objOne, clone,
				true, null);
		this.uow.getUowChangeSet().addNewObjectChangeSet(ochs);
		this.mm.mergeChangesFromChangeSet(uow.getUowChangeSet());
		assertTrue(uow.getLiveObjectCache().containsObjectByIRI(
				IRI.create(objOne.getUri())));
	}

	@Test
	public void testMergeNewObject() {
		final OWLClassB newOne = new OWLClassB();
		final URI pk = URI.create("http://newOnesUri");
		newOne.setUri(pk);
		final Object clone = cloneBuilder.buildClone(newOne);
		final ObjectChangeSetImpl ochs = new ObjectChangeSetImpl(newOne, clone,
				true, null);
		this.mm.mergeNewObject(ochs);
		assertTrue(uow.getLiveObjectCache().containsObject(newOne));
	}

	private class CloneBuilderStub extends CloneBuilderImpl {

		public CloneBuilderStub(UnitOfWorkImpl uow) {
			super(uow);
		}

		/**
		 * Does no merge, just assigns the clone to the original
		 */
		public Object mergeChanges(Object original, Object clone,
				ObjectChangeSet changeSet, MergeManager manager) {
			OWLClassB or = (OWLClassB) original;
			OWLClassB cl = (OWLClassB) clone;
			or.setStringAttribute(cl.getStringAttribute());
			return clone;
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
		 * This is the only method we need.
		 */
		public IRI getIdentifier(Object object) {
			OWLClassB ob = (OWLClassB) object;
			return IRI.create(ob.getUri());
		}

		public void persistExistingEntity(Object entity, UnitOfWork uow) {
		}

		public Query<?> createQuery(String qlString, final EntityManager em) {
			return null;
		}

		public <T> TypedQuery<T> createQuery(String query,
				Class<T> resultClass, boolean sparql, final EntityManager em) {
			return null;
		}

		public Query<List<String>> createNativeQuery(String sqlString,
				final EntityManager em) {
			return null;
		}

		public void close() {
		}

		public void generateNewIRI(Object entity) {
		}
	}
}
