package cz.cvut.kbss.jopa.accessors;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.SimpleDataSource;

public class StorageAccessorImpl implements StorageAccessor {

	private final Metamodel metamodel;
	private final ServerSession serverSession;
	private final PersistenceProviderFacade provider;

	private DataSource dataSource;
	private boolean open;

	private StorageAccessorImpl(Metamodel metamodel, ServerSession serverSession) {
		super();
		if (metamodel == null || serverSession == null) {
			throw new NullPointerException();
		}
		this.metamodel = metamodel;
		this.serverSession = serverSession;
		this.provider = initPersistenceProvider();
		this.open = true;
	}

	public StorageAccessorImpl(Metamodel metamodel, ServerSession serverSession,
			List<OntologyStorageProperties> storageProperties) {
		this(metamodel, serverSession);
		initDataSource(storageProperties, Collections.<String, String> emptyMap());
	}

	public StorageAccessorImpl(Metamodel metamodel, ServerSession serverSession,
			List<OntologyStorageProperties> storageProps, Map<String, String> properties) {
		this(metamodel, serverSession);
		initDataSource(storageProps, properties);
	}

	@Override
	public synchronized Connection acquireConnection() {
		try {
			final Connection conn = dataSource.getConnection(provider);
			conn.setAutoCommit(false);
			return conn;
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private void initDataSource(List<OntologyStorageProperties> storageProps,
			Map<String, String> properties) {
		this.dataSource = new SimpleDataSource(storageProps, properties);
	}

	private PersistenceProviderFacade initPersistenceProvider() {
		return new PersistenceProviderProxy(metamodel, serverSession);
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (dataSource != null) {
			dataSource.close();
		}
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}
}
