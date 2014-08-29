package cz.cvut.kbss.jopa.accessors;

import java.util.Map;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.DataSource;

public class NewStorageAccessor implements Closeable {

	private final DataSource dataSource;
	private boolean open;

	public NewStorageAccessor(OntologyStorageProperties storageProperties,
			Map<String, String> properties) {
		// TODO Auto-generated constructor stub
		this.dataSource = null;
		this.open = true;
	}

	public Connection acquireConnection() {
		try {
			final Connection conn = dataSource.getConnection();
			conn.setAutoCommit(false);
			return conn;
		} catch (OntoDriverException e) {
			throw new StorageAccessException("Unable to acquire storage connection.", e);
		}
	}

	@Override
	public void close() {
		if (!open) {
			return;
		}
		try {
			dataSource.close();
		} catch (OntoDriverException e) {
			throw new StorageAccessException(e);
		} finally {
			this.open = false;
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}
}
