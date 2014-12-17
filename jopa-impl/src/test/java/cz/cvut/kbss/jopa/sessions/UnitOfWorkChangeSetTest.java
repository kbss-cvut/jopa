package cz.cvut.kbss.jopa.sessions;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class UnitOfWorkChangeSetTest {

	@Mock
	private ObjectChangeSet changeSet;
	private String testObject;

	private UnitOfWorkChangeSet chs;

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Before
	public void setUp() throws Exception {
		this.testObject = "TEST";
		final String testClone = "TEST";
		MockitoAnnotations.initMocks(this);
		when(changeSet.getChangedObject()).thenReturn(testObject);
		when(changeSet.getCloneObject()).thenReturn(testClone);
		when(changeSet.getObjectClass()).thenReturn((Class) testClone.getClass());
		chs = new UnitOfWorkChangeSetImpl();
	}

	@Test
	public void testAddObjectChangeSet() {
		chs.addObjectChangeSet(changeSet);
		assertEquals(1, chs.getObjectChanges().size());
		ObjectChangeSet res = chs.getObjectChanges().iterator().next();
		assertSame(changeSet, res);
		assertTrue(chs.hasChanges());
	}

	/**
	 * This tests the fact that if we pass object change set for a new object,
	 * the UoWChangeSet should forward the call to the addNewObjectChangeSet
	 */
	@Test
	public void testAddObjectChangeSetWithNew() {
		when(changeSet.isNew()).thenReturn(Boolean.TRUE);
		chs.addObjectChangeSet(changeSet);
		assertEquals(1, chs.getNewObjects().size());
		ObjectChangeSet res = chs.getNewObjects().iterator().next();
		assertSame(changeSet, res);
		assertTrue(chs.hasNew());
	}

	@Test
	public void testAddDeletedObject() {
		chs.addDeletedObjectChangeSet(changeSet);
		assertEquals(1, chs.getDeletedObjects().size());
		ObjectChangeSet res = chs.getDeletedObjects().iterator().next();
		Object result = res.getChangedObject();
		assertEquals(testObject, result);
		assertTrue(chs.hasDeleted());
		assertTrue(chs.hasChanges());
	}

	@Test
	public void testAddNewObjectChangeSet() {
		when(changeSet.isNew()).thenReturn(Boolean.TRUE);
		chs.addNewObjectChangeSet(changeSet);
		assertTrue(chs.hasChanges());
		assertEquals(1, chs.getNewObjects().size());
		ObjectChangeSet res = chs.getNewObjects().iterator().next();
		assertSame(changeSet, res);
		assertTrue(chs.hasNew());
	}
}
