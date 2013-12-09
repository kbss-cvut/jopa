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
import org.openrdf.repository.config.RepositoryConfigException;
import org.openrdf.repository.manager.LocalRepositoryManager;
import org.openrdf.repository.manager.RepositoryProvider;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.sail.memory.MemoryStore;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.RepositoryNotFoundException;

public class SesameStorageConnector implements StorageConnector {

	private static final Logger LOG = Logger.getLogger(SesameStorageConnector.class.getName());

	private static final String[] KNOWN_SCHEMES = { "http", "https", "ftp" };
	private static final String LOCAL_NATIVE_REPO = "/repositories/";

	private final OntologyStorageProperties storageProps;

	protected String language;

	private boolean open;

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
		} catch (RepositoryException e) {
			LOG.severe("Exception caught when committing changes to the repository.");
			throw new OntoDriverException(
					"Exception caught when committing changes to repository.", e);
		}
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
			this.repository = RepositoryProvider.getRepository(serverUri.toString());
			if (repository == null) {
				if (isRemoteRepository(serverUri)) {
					throw new RepositoryNotFoundException("Unable to reach repository at "
							+ serverUri);
				} else {
					createLocalRepository(properties);
				}
			}
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
		final String useVolatileStorage = props
				.get(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE);
		if (Boolean.getBoolean(useVolatileStorage)) {
			// Use only in-memory repository, which is disposed of at shutdown
			this.repository = new SailRepository(new MemoryStore());
			return;
		}
		final String[] tmp = localUri.toString().split(LOCAL_NATIVE_REPO);
		if (tmp.length != 2) {
			throw new IllegalArgumentException(
					"Unsupported local Sesame repository path. Expected file:///path/repositories/id but got "
							+ localUri);
		}
		final File f = new File(tmp[0]);
		final String repoId = tmp[1];
		final LocalRepositoryManager m = new LocalRepositoryManager(f);
		try {
			this.repository = m.getRepository(repoId);
		} catch (RepositoryConfigException | RepositoryException e) {
			LOG.severe("Unable to create local repository at " + localUri);
			throw new OntoDriverException("Unable to create local repository.", e);
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
