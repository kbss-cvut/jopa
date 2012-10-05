package cz.cvut.kbss.jopa.owlapi.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Logger;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.accessors.OntologyAccessor;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;

public class OntologyAccessorImplTest {

	private static final Logger LOG = Logger
			.getLogger(OntologyAccessorImplTest.class.getName());
	private static final String LOCK_FIELD = "lock";

	private static EntityManagerImpl em;
	private static OntologyAccessor accessor;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		em = (EntityManagerImpl) TestEnvironment
				.getPersistenceConnector("OntologyAccessorImplTest");
		accessor = em.getServerSession().getAccessor();
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		em.getEntityManagerFactory().close();
	}

	@Test
	public void testAcquireReadLock() throws IllegalArgumentException,
			IllegalAccessException, NoSuchFieldException, SecurityException {
		LOG.info("Test: acquire read lock.");
		accessor.acquireReadLock();
		ReentrantReadWriteLock lock = getAccessorLock();
		assertEquals(1, lock.getReadLockCount());
		accessor.releaseReadLock();
	}

	@Test
	public void testReleaseReadLock() {
		LOG.info("Test: release read lock.");
		accessor.acquireReadLock();
		ReentrantReadWriteLock lock = getAccessorLock();
		assertEquals(1, lock.getReadLockCount());
		accessor.releaseReadLock();
		assertEquals(0, lock.getReadLockCount());
	}

	@Test
	public void testAcquireWriteLock() {
		LOG.info("Test: acquire write lock.");
		accessor.acquireWriteLock();
		ReentrantReadWriteLock lock = getAccessorLock();
		assertEquals(1, lock.getWriteHoldCount());
		accessor.releaseWriteLock();
	}

	@Test
	public void testReleaseWriteLock() {
		LOG.info("Test: release write lock.");
		accessor.acquireWriteLock();
		ReentrantReadWriteLock lock = getAccessorLock();
		assertEquals(1, lock.getWriteHoldCount());
		accessor.releaseWriteLock();
		assertEquals(0, lock.getWriteHoldCount());
	}

	@Test
	public void testIsOpen() {
		LOG.info("Test: is accessor open.");
		assertTrue(accessor.isOpen());
	}

	private static ReentrantReadWriteLock getAccessorLock() {
		ReentrantReadWriteLock lock = null;
		try {
			final Field lockField = accessor.getClass().getDeclaredField(
					LOCK_FIELD);
			lockField.setAccessible(true);
			lock = (ReentrantReadWriteLock) lockField.get(accessor);
		} catch (NoSuchFieldException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		}
		return lock;
	}
}
