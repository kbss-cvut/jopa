package cz.cvut.kbss.owlpersistence.owlapi.transactionsUnitTests;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSetImpl;

public class ObjectChangeSetTest {

	private UnitOfWorkChangeSet changeSet;
	private String testObject;
	private String testClone;
	
	@Before
	public void setUp() throws Exception {
		this.changeSet = new UnitOfWorkChangeSetImpl();
		this.testObject = "TEST";
		this.testClone = "TEST";
	}

	@Test
	public void testObjectChangeSetImpl() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, true, changeSet);
		assertEquals(this.testObject.getClass(), chs.getObjectClass());
	}

	@Test
	public void testAddChangeRecord() {
		final String attName = "testAtt";
		ChangeRecord record = new ChangeRecordImpl(attName, testObject);
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		chs.addChangeRecord(record);
		assertNotNull(chs.getAttributesToChange().get(attName));
		Object res = ((ChangeRecord) chs.getAttributesToChange().get(attName)).getNewValue();
		assertEquals(testObject, res);
	}

	@Test
	public void testGetChanges() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		assertNotNull(chs.getChanges());
	}

	@Test
	public void testGetAttributesToChange() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		assertNotNull(chs.getAttributesToChange());
	}

	@Test
	public void testGetObjectClass() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		assertEquals(testObject.getClass(), chs.getObjectClass());
	}

	@Test
	public void testGetChangedObject() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		assertEquals(testObject, chs.getChangedObject());
	}

	@Test
	public void testGetCloneObject() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		assertEquals(testClone, chs.getCloneObject());
	}

	@Test
	public void testSetCloneObject() {
		ObjectChangeSetImpl chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		String newClone = "newClone";
		chs.setCloneObject(newClone);
		assertNotSame(testClone, chs.getCloneObject());
	}

	@Test
	public void testSetChanges() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		List<ChangeRecord> changes = new ArrayList<ChangeRecord>();
		chs.setChanges(changes);
		assertEquals(changes, chs.getChanges());
	}

	@Test
	public void testGetUowChangeSet() {
		ObjectChangeSetImpl chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		assertEquals(changeSet, chs.getUowChangeSet());
	}

	@Test
	public void testSetUowChangeSet() {
		ObjectChangeSetImpl chs = new ObjectChangeSetImpl(testObject, testClone, false, changeSet);
		UnitOfWorkChangeSetImpl newChs = new UnitOfWorkChangeSetImpl();
		chs.setUowChangeSet(newChs);
		assertNotSame(changeSet, chs.getUowChangeSet());
	}

}
