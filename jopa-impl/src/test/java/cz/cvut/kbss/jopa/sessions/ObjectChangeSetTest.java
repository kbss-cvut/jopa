package cz.cvut.kbss.jopa.sessions;

import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

public class ObjectChangeSetTest {

	private static final URI CONTEXT = URI.create("http://example.org/uri");

	private String testObject;
	private String testClone;

	@Before
	public void setUp() throws Exception {
		this.testObject = "TEST";
		this.testClone = "TEST";
	}

	@Test
	public void testObjectChangeSetImpl() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		assertEquals(this.testObject.getClass(), chs.getObjectClass());
	}

	@Test
	public void testAddChangeRecord() {
		final String attName = "testAtt";
		ChangeRecord record = new ChangeRecordImpl(attName, testObject);
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		chs.addChangeRecord(record);
		assertNotNull(chs.getAttributesToChange().get(attName));
		Object res = chs.getAttributesToChange().get(attName).getNewValue();
		assertEquals(testObject, res);
	}

	@Test
	public void testGetChanges() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		assertNotNull(chs.getChanges());
	}

	@Test
	public void testGetAttributesToChange() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		assertNotNull(chs.getAttributesToChange());
	}

	@Test
	public void testGetObjectClass() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		assertEquals(testObject.getClass(), chs.getObjectClass());
	}

	@Test
	public void testGetChangedObject() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		assertEquals(testObject, chs.getChangedObject());
	}

	@Test
	public void testGetCloneObject() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		assertEquals(testClone, chs.getCloneObject());
	}

	@Test
	public void testSetCloneObject() {
		ObjectChangeSetImpl chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		String newClone = "newClone";
		chs.setCloneObject(newClone);
		assertNotSame(testClone, chs.getCloneObject());
	}

	@Test
	public void testSetChanges() {
		ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
		List<ChangeRecord> changes = new ArrayList<>();
		chs.setChanges(changes);
		assertEquals(changes, chs.getChanges());
	}
}
