package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * This proxy class enables us to use a single central connector and multiple
 * proxies to it. </p>
 * 
 * It is particularly useful when we have to create a local repository, so we
 * have only one repository managed by a single connector.
 * 
 * @author ledvima1
 * 
 */
class SesameStorageConnectorProxy implements SesameStorageConnector {

	private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
	private static final Lock READ = LOCK.readLock();
	private static final Lock WRITE = LOCK.writeLock();

	private final SesameStorageConnectorImpl connector;

	private boolean open = false;

	public SesameStorageConnectorProxy(SesameStorageConnectorImpl connector) {
		assert connector != null;

		this.connector = connector;
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void reload() throws OntoDriverException {
		// do nothing
	}

	@Override
	public SesameOntologyDataHolder getOntologyData() throws OntoDriverException {
		READ.lock();
		try {
			return connector.getOntologyData();
		} finally {
			READ.unlock();
		}
	}

	@Override
	public void applyChanges(List<SesameChange> changes) throws OntoDriverException {
		WRITE.lock();
		try {
			connector.applyChanges(changes);
		} finally {
			WRITE.unlock();
		}
	}

	@Override
	public long getSubjectCount() {
		// No need to lock, the subjectCount is volatile
		return connector.getSubjectCount();
	}

}
