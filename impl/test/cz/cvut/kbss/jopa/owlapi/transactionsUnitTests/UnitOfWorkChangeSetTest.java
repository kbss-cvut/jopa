package cz.cvut.kbss.jopa.owlapi.transactionsUnitTests;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSetImpl;

public class UnitOfWorkChangeSetTest {

	private ObjectChangeSetImpl newChangeSet;
	private ObjectChangeSetImpl changeSet;
	private String testObject;
	private String testClone;

	@Before
	public void setUp() throws Exception {
		this.testObject = "TEST";
		this.testClone = "TEST";
		this.newChangeSet = new ObjectChangeSetImpl(testObject, testClone,
				true, null);
		this.changeSet = new ObjectChangeSetImpl(testObject, testClone, false,
				null);
	}

	@Test
	public void testAddObjectChangeSet() {
		UnitOfWorkChangeSet chs = new UnitOfWorkChangeSetImpl();
		chs.addObjectChangeSet(changeSet);
		assertFalse(chs.getObjectChanges().isEmpty());
		Map<?, ?> trans = (Map<?, ?>) chs.getObjectChanges().get(
				changeSet.getObjectClass());
		ObjectChangeSet res = (ObjectChangeSet) trans.get(changeSet);
		assertEquals(changeSet, res);
		assertEquals(chs, changeSet.getUowChangeSet());
		// clean up
		this.changeSet.setUowChangeSet(null);
	}

	/**
	 * This tests the fact that if we pass object change set for a new object,
	 * the UoWChangeSet should forward the call to the addNewObjectChangeSet
	 */
	@Test
	public void testAddObjectChangeSetWithNew() {
		UnitOfWorkChangeSet chs = new UnitOfWorkChangeSetImpl();
		chs.addObjectChangeSet(newChangeSet);
		assertFalse(chs.getNewObjectChangeSets().isEmpty());
		Map<?, ?> trans = (Map<?, ?>) chs.getNewObjectChangeSets().get(
				newChangeSet.getObjectClass());
		ObjectChangeSet res = (ObjectChangeSet) trans.get(newChangeSet);
		assertEquals(newChangeSet, res);
		this.newChangeSet.setUowChangeSet(null);
	}

	@Test
	public void testAddDeletedObjects() {
		final String test = "TEST_2";
		final String tsClone = "TEST_2";
		Map<String, String> testMap = new HashMap<String, String>();
		testMap.put(testClone, testObject);
		testMap.put(tsClone, test);
		UnitOfWorkChangeSet chs = new UnitOfWorkChangeSetImpl();
		chs.addDeletedObjects(testMap);
		final int expectedSize = 2;
		assertEquals(expectedSize, chs.getDeletedObjects().size());
	}

	@Test
	public void testAddDeletedObject() {
		UnitOfWorkChangeSet chs = new UnitOfWorkChangeSetImpl();
		chs.addDeletedObject(testObject, testClone);
		Set<?> resSet = chs.getDeletedObjects().keySet();
		// we know there is only one change set for deleted object
		for (Object o : resSet) {
			Object result = ((ObjectChangeSet) o).getChangedObject();
			assertEquals(testObject, result);
		}
	}

	@Test
	public void testAddNewObjectChangeSet() {
		UnitOfWorkChangeSet chs = new UnitOfWorkChangeSetImpl();
		chs.addNewObjectChangeSet(newChangeSet);
		assertTrue(chs.hasChanges());
		Map<?, ?> trans = (Map<?, ?>) chs.getNewObjectChangeSets().get(
				newChangeSet.getObjectClass());
		ObjectChangeSet res = (ObjectChangeSet) trans.get(newChangeSet);
		assertEquals(newChangeSet, res);
	}

	@Test
	public void testRemoveObjectChangeSet() {
		UnitOfWorkChangeSet chs = new UnitOfWorkChangeSetImpl();
		chs.addObjectChangeSet(changeSet);
		assertTrue(chs.hasChanges());
		chs.removeObjectChangeSet(changeSet);
		assertEquals(0, chs.getObjectChanges().size());
	}

}
