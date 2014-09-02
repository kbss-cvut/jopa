package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.TransactionState;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public class PoolingStorageConnector implements Connector {

	private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
	private static final Lock READ = LOCK.readLock();
	private static final Lock WRITE = LOCK.writeLock();

	private final Connector centralConnector;
	private boolean open;
	private TransactionState transaction;
	private LocalModel localModel;

	public PoolingStorageConnector(Connector centralConnector) {
		this.centralConnector = centralConnector;
		this.transaction = TransactionState.INACTIVE;
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
	public TupleQueryResult executeQuery(String query) throws SesameDriverException {
		READ.lock();
		try {
			return centralConnector.executeQuery(query);
		} finally {
			READ.unlock();
		}
	}

	@Override
	public void executeUpdate(String query) throws SesameDriverException {
		WRITE.lock();
		try {
			centralConnector.executeUpdate(query);
		} finally {
			WRITE.unlock();
		}
	}

	@Override
	public List<Resource> getContexts() throws SesameDriverException {
		READ.lock();
		try {
			return centralConnector.getContexts();
		} finally {
			READ.unlock();
		}
	}

	@Override
	public ValueFactory getValueFactory() {
		// We don't need to lock the central connector, as getting the value
		// factory does not require communication with the repository
		return centralConnector.getValueFactory();
	}

	@Override
	public void begin() {
		if (transaction != TransactionState.INACTIVE && transaction != TransactionState.COMMITTED) {
			throw new IllegalStateException();
		}
		this.localModel = new LocalModel();
		this.transaction = TransactionState.ACTIVE;
	}

	@Override
	public void commit() throws SesameDriverException {
		verifyTransactionActive();
		this.transaction = TransactionState.PARTIALLY_COMMITTED;
		WRITE.lock();
		try {
			centralConnector.begin();
			centralConnector.addStatements(localModel.getAddedStatements());
			centralConnector.removeStatements(localModel.getRemovedStatements());
			centralConnector.commit();
			this.transaction = TransactionState.COMMITTED;
		} catch (SesameDriverException e) {
			this.transaction = TransactionState.FAILED;
			centralConnector.rollback();
		} finally {
			WRITE.unlock();
			this.localModel = null;
		}
	}

	private void verifyTransactionActive() {
		if (transaction != TransactionState.ACTIVE) {
			throw new IllegalStateException();
		}
	}

	@Override
	public void rollback() {
		verifyTransactionActive();
		this.transaction = TransactionState.FAILED;
		this.localModel = null;
		this.transaction = TransactionState.INACTIVE;
	}

	@Override
	public void addStatements(Collection<Statement> statements) {
		verifyTransactionActive();
		assert statements != null;
		localModel.addStatements(statements);
	}

	@Override
	public void removeStatements(Collection<Statement> statements) throws SesameDriverException {
		verifyTransactionActive();
		assert statements != null;
		localModel.removeStatements(statements);
	}
}
