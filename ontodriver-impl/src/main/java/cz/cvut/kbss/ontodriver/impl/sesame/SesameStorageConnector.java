package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collections;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.manager.RemoteRepositoryManager;

import cz.cvut.kbss.ontodriver.SesameOntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class SesameStorageConnector implements StorageConnector {

	private static final Logger LOG = Logger.getLogger(SesameStorageConnector.class.getName());

	private final SesameOntologyStorageProperties storageProps;

	private boolean open;

	private RemoteRepositoryManager manager;
	private Repository repository;
	private RepositoryConnection connection;

	public SesameStorageConnector(SesameOntologyStorageProperties storageProps) {
		this(storageProps, Collections.<String, String> emptyMap());
	}

	public SesameStorageConnector(SesameOntologyStorageProperties storageProps,
			Map<String, String> properties) {
		if (storageProps == null || properties == null) {
			throw new NullPointerException();
		}
		this.storageProps = storageProps;
		initialize();
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		assert manager != null;
		assert repository != null;
		assert connection != null;

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing Sesame storage connector.");
		}
		try {
			connection.close();
			repository.shutDown();
			manager.shutDown();
		} catch (RepositoryException e) {
			throw new OntoDriverException(
					"Exception caught when closing Sesame repository connection.", e);
		} finally {
			this.open = false;
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void reload() throws OntoDriverException {
		ensureOpen();
		// TODO Auto-generated method stub

	}

	private void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("The connector is closed.");
		}
	}

	private void initialize() {
		// TODO
	}
}
