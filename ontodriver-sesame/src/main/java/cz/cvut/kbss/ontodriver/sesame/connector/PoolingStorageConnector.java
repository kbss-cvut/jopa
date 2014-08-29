package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class PoolingStorageConnector implements Connector {

	private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
	private static final Lock READ = LOCK.readLock();
	private static final Lock WRITE = LOCK.writeLock();

	private final Connector centralConnector;
	private boolean open;

	public PoolingStorageConnector(Connector centralConnector) {
		this.centralConnector = centralConnector;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public TupleQueryResult executeQuery(String query) {
		READ.lock();
		try {
			return centralConnector.executeQuery(query);
		} finally {
			READ.unlock();
		}
	}

	@Override
	public void executeUpdate(String query) {
		WRITE.lock();
		try {
			centralConnector.executeUpdate(query);
		} finally {
			WRITE.unlock();
		}
	}
}
