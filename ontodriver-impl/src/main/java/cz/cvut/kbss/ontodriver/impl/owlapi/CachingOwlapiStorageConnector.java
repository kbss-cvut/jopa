package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class CachingOwlapiStorageConnector implements StorageConnector, OwlapiConnector {

	private static final Logger LOG = Logger.getLogger(CachingOwlapiStorageConnector.class
			.getName());

	private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
	private static final Lock READ = LOCK.readLock();
	private static final Lock WRITE = LOCK.writeLock();

	private final OwlapiStorageConnector connector;

	private boolean open;

	public CachingOwlapiStorageConnector(OwlapiStorageConnector connector) {
		if (connector == null) {
			throw new NullPointerException();
		}
		this.connector = connector;
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing caching OWL API storage connector.");
		}
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void reload() throws OntoDriverException {
		// Do nothing
	}

	@Override
	public void saveWorkingOntology() throws OntoDriverException {
		WRITE.lock();
		try {
			connector.saveWorkingOntology();
		} finally {
			WRITE.unlock();
		}
	}

	@Override
	public void applyChanges(List<OWLOntologyChange> changes) throws OntoDriverException {
		Objects.requireNonNull(changes, ErrorUtils.constructNPXMessage("changes"));
		if (changes.isEmpty()) {
			return;
		}
		WRITE.lock();
		try {
			connector.applyChanges(changes);
		} finally {
			WRITE.unlock();
		}
	}

	@Override
	public int getClassAssertionsCount() {
		READ.lock();
		try {
			return connector.getClassAssertionsCount();
		} finally {
			READ.unlock();
		}
	}

	/**
	 * Retrieves clone of the shared ontology and its auxiliary structures.
	 * 
	 * @return {@code OwlapiConnectorDataHolder}
	 * @throws OntoDriverException
	 *             If an error during cloning occurs
	 */
	public OwlapiConnectorDataHolder getOntologyData() throws OntoDriverException {
		READ.lock();
		try {
			return connector.cloneOntologyData();
		} finally {
			READ.unlock();
		}
	}
}
