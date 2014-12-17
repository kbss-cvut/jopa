package cz.cvut.kbss.jopa.sessions;

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

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;

public class MergeManagerTest {

	private static final URI DEFAULT_URI = URI.create("http://defaultContext");

	private static Descriptor defaultDescriptor;

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
		defaultDescriptor = new EntityDescriptor(DEFAULT_URI);
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
	public void testMergeChangesOnObject() throws Exception {
		final OWLClassB orig = new OWLClassB();
		final URI pk = URI.create("http://testObject");
		orig.setUri(pk);
		orig.setStringAttribute("ANiceAttribute");
		final OWLClassB clone = (OWLClassB) cloneBuilder.buildClone(orig, defaultDescriptor);
		final ObjectChangeSet chs = createChangeSet(orig, clone);
		clone.setStringAttribute("AnotherStringAttribute");
		chs.addChangeRecord(new ChangeRecordImpl(OWLClassB.getStrAttField().getName(), clone
				.getStringAttribute()));
		mm.mergeChangesOnObject(clone, chs);
		assertEquals(clone.getStringAttribute(), orig.getStringAttribute());
	}

	@Test
	public void testMergeChangesFromChangeSet() throws Exception {
		final OWLClassB objOne = new OWLClassB();
		final URI pk = URI.create("http://objOne");
		objOne.setUri(pk);
		final OWLClassB objTwo = new OWLClassB();
		final URI pkTwo = URI.create("http://objTwo");
		objTwo.setUri(pkTwo);
		OWLClassB cloneOne = (OWLClassB) cloneBuilder.buildClone(objOne, defaultDescriptor);
		OWLClassB cloneTwo = (OWLClassB) cloneBuilder.buildClone(objTwo, defaultDescriptor);
		cloneOne.setStringAttribute("testAtt");
		uowChangeSet.addDeletedObjectChangeSet(createChangeSet(objTwo, cloneTwo));
		final ObjectChangeSet ochs = createChangeSet(objOne, cloneOne);
		ochs.addChangeRecord(new ChangeRecordImpl(OWLClassB.getStrAttField().getName(), cloneOne
				.getStringAttribute()));
		uowChangeSet.addObjectChangeSet(ochs);
		mm.mergeChangesFromChangeSet(uowChangeSet);
		verify(uow).removeObjectFromCache(objTwo, defaultDescriptor.getContext());
		assertEquals(((OWLClassB) cloneOne).getStringAttribute(), objOne.getStringAttribute());
	}

	@Test
	public void testMergeChangesFromChangeSetWithNew() {
		final OWLClassB objOne = new OWLClassB();
		final URI pk = URI.create("http://newOnesUri");
		objOne.setUri(pk);
		objOne.setStringAttribute("ABeautifulAttribute");
		final OWLClassB clone = (OWLClassB) cloneBuilder.buildClone(objOne, defaultDescriptor);
		final ObjectChangeSet ochs = createChangeSet(objOne, clone);
		uowChangeSet.addNewObjectChangeSet(ochs);
		mm.mergeChangesFromChangeSet(uowChangeSet);
		verify(uow).putObjectIntoCache(IRI.create(pk), objOne, defaultDescriptor.getContext());
	}

	@Test
	public void testMergeNewObject() {
		final OWLClassB newOne = new OWLClassB();
		final URI pk = URI.create("http://newOnesUri");
		newOne.setUri(pk);
		final OWLClassB clone = (OWLClassB) cloneBuilder.buildClone(newOne, defaultDescriptor);
		final ObjectChangeSet ochs = createChangeSet(newOne, clone);
		mm.mergeNewObject(ochs);
		verify(uow).putObjectIntoCache(IRI.create(pk), newOne, defaultDescriptor.getContext());
	}

	private static ObjectChangeSet createChangeSet(OWLClassB orig, OWLClassB clone) {
		return TestEnvironmentUtils.createObjectChangeSet(orig, clone,
				defaultDescriptor.getContext());
	}

	private static class CloneBuilderStub extends CloneBuilderImpl {

		public CloneBuilderStub(UnitOfWorkImpl uow) {
			super(uow);
		}

		/**
		 * Does no merge, just assigns the clone to the original
		 */
		public Object mergeChanges(Object original, ObjectChangeSet changeSet) {
			OWLClassB or = (OWLClassB) original;
			ChangeRecord change;
			try {
				change = changeSet.getAttributesToChange()
						.get(OWLClassB.getStrAttField().getName());
				if (change != null) {
					or.setStringAttribute((String) change.getNewValue());
				}
			} catch (NoSuchFieldException | SecurityException e) {
				throw new OWLPersistenceException(e);
			}
			return or;
		}
	}
}
