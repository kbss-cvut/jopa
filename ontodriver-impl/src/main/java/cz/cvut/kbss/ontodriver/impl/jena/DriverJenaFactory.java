package cz.cvut.kbss.ontodriver.impl.jena;

import java.io.File;
import java.net.URI;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiBasedJenaModule;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStatement;

public class DriverJenaFactory extends DriverAbstractFactory {

	private static final String JDBC_SCHEME = "jdbc";

	public DriverJenaFactory(OntologyStorageProperties repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositoryProperties, properties);
	}

	@Override
	public StorageModule createStorageModule(PersistenceProviderFacade persistenceProvider,
			boolean autoCommit) throws OntoDriverException {
		ensureParametersAndState(persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Jena storage module.");
		}
		final StorageModule m = new OwlapiBasedJenaModule(persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public JenaStorageConnector createStorageConnector(boolean autoCommit)
			throws OntoDriverException {
		ensureOpen();
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Jena storage connector.");
		}
		final JenaStorageType storageType = resolveStorageType(storageProperties);
		JenaStorageConnector c = null;
		switch (storageType) {
		case FILE:
			c = new JenaFileStorageConnector(storageProperties, properties);
			break;
		case TDB:
			c = new JenaTDBStorageConnector(storageProperties, properties);
			break;
		default:
			throw new IllegalArgumentException("Unsupported storage type " + storageType);
		}
		registerConnector(c);
		return c;
	}

	/**
	 * Resolves Jena storage type based on its physical URI. </p>
	 * 
	 * The resolving follows these principles:
	 * <ul>
	 * <li>If the physical URI is a JDBC address, i. e. the URI scheme is
	 * 'jdbc', the storage is Jena SDB.</li>
	 * <li>If the physical URI points to an existing directory, the storage is
	 * Jena TDB.</li>
	 * <li>If the physical URI points to an existing file, the storage is a
	 * file.</li>
	 * <li>If the target doesn't exist and the path ends with a file system
	 * separator, e. g. / on UNIX or \ on Windows, the storage is Jena TDB.</li>
	 * <li>Otherwise the storage is a file.</li>
	 * </ul>
	 * 
	 * @param properties
	 *            Storage properties
	 * @return {@code JenaStorageType}
	 */
	public static JenaStorageType resolveStorageType(OntologyStorageProperties properties) {
		Objects.requireNonNull(properties, ErrorUtils.constructNPXMessage("properties"));
		final URI uri = properties.getPhysicalURI();
		if (uri.getScheme().equals(JDBC_SCHEME)) {
			return JenaStorageType.SDB;
		}
		final File f = new File(uri);
		if (f.exists()) {
			if (f.isDirectory()) {
				return JenaStorageType.TDB;
			} else {
				return JenaStorageType.FILE;
			}
		} else {
			final String strUri = uri.toString();
			if (strUri.charAt(strUri.length() - 1) == File.separatorChar) {
				return JenaStorageType.TDB;
			} else {
				return JenaStorageType.FILE;
			}
		}
	}

	@Override
	public DriverStatement createStatement(JopaStatement statement) throws OntoDriverException {
		Objects.requireNonNull(statement, ErrorUtils.constructNPXMessage("statement"));
		return new OwlapiStatement(statement);
	}
}
