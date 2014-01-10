package cz.cvut.kbss.ontodriver.impl.sesame;

import info.aduna.iteration.Iterations;

import java.io.File;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.model.Model;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.LinkedHashModel;
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
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.RepositoryCreationException;
import cz.cvut.kbss.ontodriver.exceptions.RepositoryNotFoundException;

public class SesameStorageConnector implements StorageConnector {

	private static final Logger LOG = Logger.getLogger(SesameStorageConnector.class.getName());

	private static final String[] KNOWN_SCHEMES = { "http", "https", "ftp" };
	private static final String LOCAL_NATIVE_REPO = "/repositories/";
	private static final String FILE_SCHEME = "file";

	private final OntologyStorageProperties storageProps;

	protected String language;

	private boolean open;
	private long currentSubjectCount;

	private RepositoryManager manager;
	private Repository repository;
	private RepositoryConnection connection;

	public SesameStorageConnector(OntologyStorageProperties storageProps)
			throws OntoDriverException {
		this(storageProps, Collections.<String, String> emptyMap());
	}

	public SesameStorageConnector(OntologyStorageProperties storageProps,
			Map<String, String> properties) throws OntoDriverException {
		if (storageProps == null || properties == null) {
			throw new NullPointerException();
		}
		this.storageProps = storageProps;
		this.currentSubjectCount = -1;
		initialize(properties);
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		assert repository != null;
		assert connection != null;

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing Sesame storage connector.");
		}
		closeRepository();
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void reload() throws OntoDriverException {
		ensureOpen();
		try {
			connection.close();
			this.connection = repository.getConnection();
		} catch (RepositoryException e) {
			LOG.severe("Failed to reload repository connection.");
			throw new OntoDriverException("Failed to reload repository connection.", e);
		}
	}

	/**
	 * Returns a copy of the current state of the underlying ontology. </p>
	 * 
	 * @param includeInferred
	 *            Specifies whether inferred statements should be included in
	 *            the returned data (assuming that the underlying repository
	 *            supports inference)
	 * @return Data holder containing copy of the RDF graph model with explicit
	 *         statements, optionally another containing inferred statements as
	 *         well and a {@link ValueFactory}
	 * @throws OntoDriverException
	 */
	public SesameOntologyDataHolder getOntologyData(boolean includeInferred)
			throws OntoDriverException {
		ensureOpen();
		try {
			Model model = null;
			if (includeInferred) {
				// Get all statements from the repository
				final RepositoryResult<Statement> statements = connection.getStatements(null, null,
						null, includeInferred);
				model = Iterations.addAll(statements, new LinkedHashModel());
			}
			final RepositoryResult<Statement> explicitStatements = connection.getStatements(null,
					null, null, false);
			final Model explicitModel = Iterations
					.addAll(explicitStatements, new LinkedHashModel());
			this.currentSubjectCount = explicitModel.size();
			final ValueFactory vf = connection.getValueFactory();
			return SesameOntologyDataHolder.model(model).explicitModel(explicitModel)
					.valueFactory(vf).language(language).build();
		} catch (RepositoryException e) {
			throw new OntoDriverException(
					"Exception caught when extracting statements from repository.", e);
		}
	}

	/**
	 * Gets a connection to the underlying repository. </p>
	 * 
	 * The caller is expected to handle all the transaction synchronization and
	 * connection lifecycle.
	 * 
	 * @return Connection to the underlying Sesame repository
	 * @throws OntoDriverException
	 *             If the connector is unable to get connection from the
	 *             repository
	 */
	public RepositoryConnection getRepositoryConnection() throws OntoDriverException {
		ensureOpen();
		try {
			return repository.getConnection();
		} catch (RepositoryException e) {
			throw new OntoDriverException("Unable to get connection to repository " + repository, e);
		}
	}

	public void applyChanges(List<SesameChange> changes) throws OntoDriverException {
		assert changes != null;
		ensureOpen();
		if (changes.isEmpty()) {
			return;
		}
		assert connection != null;
		try {
			assert connection.isOpen();
			connection.begin();
			for (SesameChange ch : changes) {
				ch.apply(connection);
			}
			connection.commit();
			this.currentSubjectCount = -1;
		} catch (RepositoryException e) {
			LOG.severe("Exception caught when committing changes to the repository.");
			throw new OntoDriverException(
					"Exception caught when committing changes to repository.", e);
		}
	}

	/**
	 * Get the current count of subjects in the ontology.
	 * 
	 * @return
	 * @throws RepositoryException
	 */
	public long getSubjectCount() throws OntoDriverException {
		if (currentSubjectCount < 0) {
			try {
				this.currentSubjectCount = repository.getConnection().size();
			} catch (RepositoryException e) {
				throw new OntoDriverException("Unable to resolve statement count in repository.", e);
			}
		}
		return currentSubjectCount;
	}

	private void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("The connector is closed.");
		}
	}

	private void initialize(Map<String, String> properties) throws OntoDriverException {
		final URI serverUri = storageProps.getPhysicalURI();
		this.language = properties.get(OntoDriverProperties.ONTOLOGY_LANGUAGE);
		try {
			boolean isRemote = false;
			isRemote = isRemoteRepository(serverUri);
			if (isRemote) {
				this.repository = RepositoryProvider.getRepository(serverUri.toString());
			} else {
				createLocalRepository(properties);
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
			this.connection = repository.getConnection();
		} catch (RepositoryException | RepositoryConfigException e) {
			LOG.severe("Failed to acquire Sesame repository connection.");
			throw new OntoDriverException("Failed to acquire sesame repository connection.", e);
		}
	}

	private void closeRepository() throws OntoDriverException {
		try {
			connection.close();
			repository.shutDown();
			if (manager != null) {
				manager.shutDown();
			}
		} catch (RepositoryException e) {
			throw new OntoDriverException(
					"Exception caught when closing Sesame repository connection.", e);
		}
	}

	/**
	 * Creates a local repository at the specified URI. </p>
	 * 
	 * @param props
	 *            Properties
	 * @throws OntoDriverException
	 *             If the repository cannot be created
	 */
	private void createLocalRepository(Map<String, String> props) throws OntoDriverException {
		final URI localUri = storageProps.getPhysicalURI();
		boolean useVolatile = Boolean.parseBoolean(props
				.get(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE));
		if (!isFileUri(localUri) && useVolatile) {
			this.repository = createInMemoryRepository(props);
			return;
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
		final RepositoryConfig cfg = createRepositoryConfig(repoId, props);
		try {
			manager.initialize();
			manager.addRepositoryConfig(cfg);
			this.repository = manager.getRepository(repoId);
		} catch (RepositoryConfigException | RepositoryException e) {
			LOG.severe("Unable to create local repository at " + localUri);
			throw new OntoDriverException("Unable to create local repository.", e);
		}
	}

	/**
	 * Creates repository configuration for local native Sesame repository.
	 * 
	 * @param repoId
	 *            Repository id
	 * @param props
	 *            Properties map
	 * @return Repository configuration
	 */
	private RepositoryConfig createRepositoryConfig(String repoId, Map<String, String> props) {
		SailImplConfig backend = new NativeStoreConfig();
		if (shouldUseInferenceInLocal(props)) {
			backend = new ForwardChainingRDFSInferencerConfig(backend);
		}
		final SailRepositoryConfig repoType = new SailRepositoryConfig(backend);
		return new RepositoryConfig(repoId, repoType);
	}

	/**
	 * Returns true if inference should be used in local repositories.
	 * 
	 * @param properties
	 * @return
	 */
	private static boolean shouldUseInferenceInLocal(Map<String, String> properties) {
		assert properties != null;
		// We can use directly this, because Boolean.parseBoolean return false
		// for null
		return (Boolean.parseBoolean(properties.get(OntoDriverProperties.SESAME_USE_INFERENCE)));
	}

	private static boolean isFileUri(URI uri) {
		return (uri.getScheme() != null && uri.getScheme().equals(FILE_SCHEME));
	}

	/**
	 * Creates a local in-memory Sesame repository which is disposed when the VM
	 * shuts down.
	 * 
	 * @return Repository
	 */
	private static Repository createInMemoryRepository(Map<String, String> props) {
		final MemoryStore ms = new MemoryStore();
		if (shouldUseInferenceInLocal(props)) {
			return new SailRepository(new ForwardChainingRDFSInferencer(ms));
		} else {
			return new SailRepository(ms);
		}
	}

	private static boolean isRemoteRepository(URI uri) {
		final String scheme = uri.getScheme();
		for (String s : KNOWN_SCHEMES) {
			if (s.equals(scheme)) {
				return true;
			}
		}
		return false;
	}
}
