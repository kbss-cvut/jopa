package cz.cvut.kbss.ontodriver.impl.sesame;

import java.io.File;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.model.ValueFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
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
	private final Map<String, String> props;

	protected String language;

	private volatile boolean open;
	private volatile long currentSubjectCount;

	private RepositoryManager manager;
	private Repository repository;

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
		this.props = properties;
		this.currentSubjectCount = -1;
		initialize();
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		assert repository != null;

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
		// no-op
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
	public SesameOntologyDataHolder getOntologyData() throws OntoDriverException {
		ensureOpen();
		try {
			final RepositoryConnection conn = repository.getConnection();
			this.currentSubjectCount = conn.size();
			final ValueFactory vf = conn.getValueFactory();
			return SesameOntologyDataHolder.storage(createStorageProxy(conn)).valueFactory(vf)
					.language(language).build();
		} catch (RepositoryException e) {
			throw new OntoDriverException(
					"Exception caught when extracting statements from repository.", e);
		}
	}

	public void applyChanges(List<SesameChange> changes) throws OntoDriverException {
		assert changes != null;
		ensureOpen();
		if (changes.isEmpty()) {
			return;
		}
		RepositoryConnection connection;
		try {
			connection = repository.getConnection();
		} catch (RepositoryException ex) {
			throw new OntoDriverException("Unable to open repository connection.", ex);
		}
		try {
			connection.begin();
			for (SesameChange ch : changes) {
				ch.apply(connection);
			}
			connection.commit();
			this.currentSubjectCount = connection.size();
		} catch (RepositoryException e) {
			LOG.severe("Exception caught when committing changes to the repository.");
			throw new OntoDriverException(
					"Exception caught when committing changes to repository.", e);
		} finally {
			try {
				connection.close();
			} catch (RepositoryException e) {
				LOG.log(Level.SEVERE, "Exception caught when closing repository connection.", e);
			}
		}
	}

	/**
	 * Get the current count of subjects in the ontology.
	 * 
	 * @return
	 * @throws RepositoryException
	 */
	public long getSubjectCount() throws OntoDriverException {
		return currentSubjectCount;
	}

	private void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("The connector is closed.");
		}
	}

	private void initialize() throws OntoDriverException {
		final URI serverUri = storageProps.getPhysicalURI();
		this.language = props.get(OntoDriverProperties.ONTOLOGY_LANGUAGE);
		try {
			boolean isRemote = false;
			isRemote = isRemoteRepository(serverUri);
			if (isRemote) {
				this.repository = RepositoryProvider.getRepository(serverUri.toString());
			} else {
				createLocalRepository();
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
			LOG.severe("Failed to acquire Sesame repository connection.");
			throw new OntoDriverException("Failed to acquire sesame repository connection.", e);
		}
	}

	private void closeRepository() throws OntoDriverException {
		try {
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
	private void createLocalRepository() throws OntoDriverException {
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

	private StorageProxy createStorageProxy(RepositoryConnection conn) throws OntoDriverException {
		if (Boolean.parseBoolean(props.get(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY))) {
			return new CachingStorageProxy(conn);
		} else {
			return new TransparentStorageProxy(conn);
		}
	}

	/**
	 * Returns true if inference should be used in local repositories.
	 * 
	 * @param properties
	 * @return
	 */
	private static boolean shouldUseInferenceInLocal(Map<String, String> properties) {
		assert properties != null;
		// We can use directly this, because Boolean.parseBoolean returns false
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
