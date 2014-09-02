package cz.cvut.kbss.ontodriver.sesame.connector;

import info.aduna.iteration.Iterations;

import java.io.File;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.query.MalformedQueryException;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.TupleQuery;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.query.Update;
import org.openrdf.query.UpdateExecutionException;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.RepositoryResult;
import org.openrdf.repository.config.RepositoryConfig;
import org.openrdf.repository.config.RepositoryConfigException;
import org.openrdf.repository.manager.LocalRepositoryManager;
import org.openrdf.repository.manager.RepositoryManager;
import org.openrdf.repository.manager.RepositoryProvider;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.repository.sail.config.SailRepositoryConfig;
import org.openrdf.sail.config.SailImplConfig;
import org.openrdf.sail.inferencer.fc.ForwardChainingRDFSInferencer;
import org.openrdf.sail.inferencer.fc.config.ForwardChainingRDFSInferencerConfig;
import org.openrdf.sail.memory.MemoryStore;
import org.openrdf.sail.nativerdf.config.NativeStoreConfig;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.sesame.exceptions.RepositoryCreationException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.RepositoryNotFoundException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

class StorageConnector implements Connector {

	private static final String[] KNOWN_REMOTE_SCHEMES = { "http", "https", "ftp" };
	private static final String LOCAL_NATIVE_REPO = "/repositories/";
	private static final String FILE_SCHEME = "file";

	private static final Logger LOG = Logger.getLogger(StorageConnector.class.getName());

	private final OntologyStorageProperties storageProperties;
	private boolean open;

	private Repository repository;
	private RepositoryManager manager;

	public StorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws SesameDriverException {
		assert storageProperties != null;
		assert properties != null;

		this.storageProperties = storageProperties;
		initialize(properties);
		this.open = true;
	}

	private void initialize(Map<String, String> properties) throws SesameDriverException {
		final URI serverUri = storageProperties.getPhysicalURI();
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Initializing connector to repository at " + serverUri);
		}
		try {
			final boolean isRemote = isRemoteRepository(serverUri);
			if (isRemote) {
				this.repository = RepositoryProvider.getRepository(serverUri.toString());
			} else {
				this.repository = createLocalRepository(properties);
			}
			if (repository == null) {
				if (isRemote) {
					throw new RepositoryNotFoundException("Unable to reach repository at "
							+ serverUri);
				} else {
					throw new RepositoryCreationException("Unable to create local repository at "
							+ serverUri);
				}
			}
			repository.initialize();
		} catch (RepositoryException | RepositoryConfigException e) {
			throw new SesameDriverException("Failed to acquire sesame repository connection.", e);
		}
	}

	private Repository createLocalRepository(Map<String, String> props) {
		final URI localUri = storageProperties.getPhysicalURI();
		boolean useVolatile = Boolean.parseBoolean(props
				.get(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE));
		if (!isFileUri(localUri) && useVolatile) {
			return createInMemoryRepository(props);
		} else {
			return createNativeRepository(props, localUri);
		}
	}

	/**
	 * Creates a local in-memory Sesame repository which is disposed when the VM
	 * shuts down.
	 */
	private Repository createInMemoryRepository(Map<String, String> props) {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Creating local in-memory repository.");
		}
		final MemoryStore ms = new MemoryStore();
		if (shouldUseInferenceInLocalRepositories(props)) {
			return new SailRepository(new ForwardChainingRDFSInferencer(ms));
		} else {
			return new SailRepository(ms);
		}
	}

	/**
	 * Creates native repository. </p>
	 * 
	 * This kind of repository stores data in files and is persistent after the
	 * VM shuts down.
	 */
	private Repository createNativeRepository(Map<String, String> props, final URI localUri) {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Creating local native repository at " + localUri);
		}
		final String[] tmp = localUri.toString().split(LOCAL_NATIVE_REPO);
		if (tmp.length != 2) {
			throw new IllegalArgumentException(
					"Unsupported local Sesame repository path. Expected file:/path/repositories/id but got "
							+ localUri);
		}
		final File f = new File(URI.create(tmp[0]));
		String repoId = tmp[1];
		if (repoId.charAt(repoId.length() - 1) == '/') {
			repoId = repoId.substring(0, repoId.length() - 1);
		}
		this.manager = new LocalRepositoryManager(f);
		final RepositoryConfig cfg = createLocalNativeRepositoryConfig(repoId, props);
		try {
			manager.initialize();
			manager.addRepositoryConfig(cfg);
			return manager.getRepository(repoId);
		} catch (RepositoryConfigException | RepositoryException e) {
			throw new RepositoryCreationException("Unable to create local repository at "
					+ localUri, e);
		}
	}

	private RepositoryConfig createLocalNativeRepositoryConfig(String repoId,
			Map<String, String> props) {
		SailImplConfig backend = new NativeStoreConfig();
		if (shouldUseInferenceInLocalRepositories(props)) {
			backend = new ForwardChainingRDFSInferencerConfig(backend);
		}
		final SailRepositoryConfig repoType = new SailRepositoryConfig(backend);
		return new RepositoryConfig(repoId, repoType);
	}

	@Override
	public void close() throws SesameDriverException {
		if (!open) {
			return;
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing connector to repository " + storageProperties.getPhysicalURI());
		}
		try {
			repository.shutDown();
			if (manager != null) {
				manager.shutDown();
			}
		} catch (RepositoryException e) {
			throw new SesameDriverException(
					"Exception caught when closing Sesame repository connection.", e);
		} finally {
			this.open = false;
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	private static boolean isFileUri(URI uri) {
		return (uri.getScheme() != null && uri.getScheme().equals(FILE_SCHEME));
	}

	private static boolean shouldUseInferenceInLocalRepositories(Map<String, String> properties) {
		assert properties != null;

		return (Boolean.parseBoolean(properties.get(OntoDriverProperties.SESAME_USE_INFERENCE)));
	}

	private static boolean isRemoteRepository(URI uri) {
		final String scheme = uri.getScheme();
		for (String s : KNOWN_REMOTE_SCHEMES) {
			if (s.equals(scheme)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public TupleQueryResult executeQuery(String query) throws SesameDriverException {
		RepositoryConnection connection = null;
		try {
			connection = acquireConnection();
			final TupleQuery tq = connection.prepareTupleQuery(QueryLanguage.SPARQL, query);
			return tq.evaluate();
		} catch (MalformedQueryException | QueryEvaluationException | RepositoryException e) {
			throw new SesameDriverException(e);
		} finally {
			try {
				connection.close();
			} catch (RepositoryException e) {
				throw new SesameDriverException(e);
			}
		}
	}

	private RepositoryConnection acquireConnection() throws SesameDriverException {
		try {
			return repository.getConnection();
		} catch (RepositoryException e) {
			throw new SesameDriverException(e);
		}
	}

	@Override
	public void executeUpdate(String query) throws SesameDriverException {
		RepositoryConnection connection = null;
		try {
			connection = acquireConnection();
			final Update u = connection.prepareUpdate(QueryLanguage.SPARQL, query);
			u.execute();
		} catch (MalformedQueryException | UpdateExecutionException | RepositoryException e) {
			throw new SesameDriverException(e);
		} finally {
			try {
				connection.close();
			} catch (RepositoryException e) {
				throw new SesameDriverException(e);
			}
		}
	}

	@Override
	public List<Resource> getContexts() throws SesameDriverException {
		RepositoryConnection connection = null;
		try {
			connection = acquireConnection();
			final RepositoryResult<Resource> res = connection.getContextIDs();
			return Iterations.asList(res);
		} catch (RepositoryException e) {
			throw new SesameDriverException(e);
		} finally {
			try {
				connection.close();
			} catch (RepositoryException e) {
				throw new SesameDriverException(e);
			}
		}
	}

	@Override
	public ValueFactory getValueFactory() {
		return repository.getValueFactory();
	}

	@Override
	public void commit() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void rollback() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void addStatements(Collection<Statement> statements) {
		// TODO Auto-generated method stub
		
	}
}
