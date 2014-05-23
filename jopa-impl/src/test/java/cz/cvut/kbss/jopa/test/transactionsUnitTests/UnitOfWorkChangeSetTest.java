package cz.cvut.kbss.jopa.test.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSetImpl;

public class UnitOfWorkChangeSetTest {

	@Mock
	private ObjectChangeSet changeSet;
	private String testObject;
	private String testClone;

	private UnitOfWorkChangeSet chs;

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Before
	public void setUp() throws Exception {
		this.testObject = "TEST";
		this.testClone = "TEST";
		MockitoAnnotations.initMocks(this);
		when(changeSet.getChangedObject()).thenReturn(testObject);
		when(changeSet.getCloneObject()).thenReturn(testClone);
		when(changeSet.getObjectClass()).thenReturn((Class) testClone.getClass());
		chs = new UnitOfWorkChangeSetImpl();
	}

	@Test
	public void testAddObjectChangeSet() {
		chs.addObjectChangeSet(changeSet);
		assertFalse(chs.getObjectChanges().isEmpty());
		verify(changeSet, atLeastOnce()).getObjectClass();
		Map<?, ?> trans = (Map<?, ?>) chs.getObjectChanges().get(changeSet.getObjectClass());
		ObjectChangeSet res = (ObjectChangeSet) trans.get(changeSet);
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
		assertFalse(chs.getNewObjectChangeSets().isEmpty());
		Map<?, ?> trans = (Map<?, ?>) chs.getNewObjectChangeSets().get(changeSet.getObjectClass());
		ObjectChangeSet res = (ObjectChangeSet) trans.get(changeSet);
		assertSame(changeSet, res);
		assertTrue(chs.hasNew());
	}

	@Test
	public void testAddDeletedObject() {
		chs.addDeletedObject(changeSet);
		Set<?> resSet = chs.getDeletedObjects().keySet();
		assertEquals(1, resSet.size());
		ObjectChangeSet res = (ObjectChangeSet) resSet.iterator().next();
		Object result = ((ObjectChangeSet) res).getChangedObject();
		assertEquals(testObject, result);
		assertTrue(chs.hasDeletedObjects());
		assertTrue(chs.hasChanges());
	}

	@Test
	public void testAddNewObjectChangeSet() {
		when(changeSet.isNew()).thenReturn(Boolean.TRUE);
		chs.addNewObjectChangeSet(changeSet);
		assertTrue(chs.hasChanges());
		Map<?, ?> trans = (Map<?, ?>) chs.getNewObjectChangeSets().get(changeSet.getObjectClass());
		ObjectChangeSet res = (ObjectChangeSet) trans.get(changeSet);
		assertSame(changeSet, res);
		assertTrue(chs.hasNew());
	}

	@Test
	public void testRemoveObjectChangeSet() {
		chs.addObjectChangeSet(changeSet);
		assertTrue(chs.hasChanges());
		chs.removeObjectChangeSet(changeSet);
		assertEquals(0, chs.getObjectChanges().size());
	}

}
