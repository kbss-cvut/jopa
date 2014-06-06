package cz.cvut.kbss.jopa.test.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.CloneBuilderImpl;
import cz.cvut.kbss.jopa.sessions.MergeManager;
import cz.cvut.kbss.jopa.sessions.MergeManagerImpl;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSetImpl;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;

public class MergeManagerTest {

	private static final URI DEFAULT_URI = URI.create("http://defaultContext");

	private static EntityDescriptor defaultDescriptor;

	@Mock
	private UnitOfWorkImpl uow;

	@Mock
	private Metamodel metamodel;

	@Mock
	private EntityType<OWLClassB> et;

	@Mock
	private Identifier identifier;

	private CloneBuilderStub cloneBuilder;
	private UnitOfWorkChangeSet uowChangeSet;

	private MergeManagerImpl mm;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		defaultDescriptor = new EntityDescriptor();
		defaultDescriptor.setEntityContext(DEFAULT_URI);
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		this.cloneBuilder = new CloneBuilderStub(uow);
		this.uowChangeSet = new UnitOfWorkChangeSetImpl();
		mm = new MergeManagerImpl(uow);
		// Set the stub as the clone builder
		Field builder = mm.getClass().getDeclaredField("builder");
		builder.setAccessible(true);
		builder.set(mm, cloneBuilder);
		when(uow.getMetamodel()).thenReturn(metamodel);
		when(metamodel.entity(OWLClassB.class)).thenReturn(et);
		when(et.getIdentifier()).thenReturn(identifier);
		when(identifier.getJavaField()).thenReturn(OWLClassB.class.getDeclaredField("uri"));
	}

	@After
	public void tearDown() throws Exception {
		uow.release();
	}

	@Test
	public void testMergeChangesOnObject() {
		final OWLClassB orig = new OWLClassB();
		final URI pk = URI.create("http://testObject");
		orig.setUri(pk);
		orig.setStringAttribute("ANiceAttribute");
		final OWLClassB clone = (OWLClassB) cloneBuilder.buildClone(orig, defaultDescriptor);
		final ObjectChangeSet chs = createChangeSet(orig, clone);
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
		Object cloneOne = cloneBuilder.buildClone(objOne, defaultDescriptor);
		Object cloneTwo = cloneBuilder.buildClone(objTwo, defaultDescriptor);
		((OWLClassB) cloneOne).setStringAttribute("testAtt");
		uowChangeSet.addDeletedObject(createChangeSet(objTwo, cloneTwo));
		final ObjectChangeSet ochs = createChangeSet(objOne, cloneOne);
		uowChangeSet.addObjectChangeSet(ochs);
		mm.mergeChangesFromChangeSet(uowChangeSet);
		verify(uow).removeObjectFromCache(objTwo, defaultDescriptor.getEntityContext());
		assertEquals(((OWLClassB) cloneOne).getStringAttribute(), objOne.getStringAttribute());
	}

	@Test
	public void testMergeChangesFromChangeSetWithNew() {
		final OWLClassB objOne = new OWLClassB();
		final URI pk = URI.create("http://newOnesUri");
		objOne.setUri(pk);
		objOne.setStringAttribute("ABeautifulAttribute");
		final Object clone = cloneBuilder.buildClone(objOne, defaultDescriptor);
		final ObjectChangeSet ochs = createChangeSet(objOne, clone);
		uowChangeSet.addNewObjectChangeSet(ochs);
		mm.mergeChangesFromChangeSet(uowChangeSet);
		verify(uow)
				.putObjectIntoCache(IRI.create(pk), objOne, defaultDescriptor.getEntityContext());
	}

	@Test
	public void testMergeNewObject() {
		final OWLClassB newOne = new OWLClassB();
		final URI pk = URI.create("http://newOnesUri");
		newOne.setUri(pk);
		final Object clone = cloneBuilder.buildClone(newOne, defaultDescriptor);
		final ObjectChangeSet ochs = createChangeSet(newOne, clone);
		mm.mergeNewObject(ochs);
		verify(uow)
				.putObjectIntoCache(IRI.create(pk), newOne, defaultDescriptor.getEntityContext());
	}

	private static ObjectChangeSet createChangeSet(Object orig, Object clone) {
		return TestEnvironmentUtils.createObjectChangeSet(orig, clone,
				defaultDescriptor.getEntityContext());
	}

	private static class CloneBuilderStub extends CloneBuilderImpl {

		public CloneBuilderStub(UnitOfWorkImpl uow) {
			super(uow);
		}

		/**
		 * Does no merge, just assigns the clone to the original
		 */
		public Object mergeChanges(Object original, Object clone, ObjectChangeSet changeSet,
				MergeManager manager) {
			OWLClassB or = (OWLClassB) original;
			OWLClassB cl = (OWLClassB) clone;
			or.setStringAttribute(cl.getStringAttribute());
			return clone;
		}
	}
}
