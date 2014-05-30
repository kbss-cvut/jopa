package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import de.fraunhofer.iitb.owldb.OWLDBManager;

/**
 * This factory provides better optimized OWL API ontology module extracting
 * modules and connectors.
 * 
 * @author kidney
 * 
 */
public class DriverModularizingOwlapiFactory extends DriverAbstractFactory {

	private volatile OwlapiStorageConnector centralConnector;
	private boolean owldb = false;

	public DriverModularizingOwlapiFactory(OntologyStorageProperties repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositoryProperties, properties);
	}

	@Override
	public ModularizingOwlapiStorageModule createStorageModule(
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureParametersAndState(persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage module.");
		}
		final ModularizingOwlapiStorageModule m = new ModularizingOwlapiStorageModule(
				persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public ModularizingStorageConnector createStorageConnector(boolean autoCommit)
			throws OntoDriverException {
		ensureOpen();
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI modularizing storage connector.");
		}
		return createConnectorInternal(autoCommit);
	}

	/**
	 * Creates modularizing storage connector. </p>
	 * 
	 * This method has to be synchronized since it performs a put-if-absent
	 * operation on the {@code centralConnectors}, which is not thread safe.
	 * 
	 * @param ctx
	 * @param autoCommit
	 * @return
	 * @throws OntoDriverException
	 */
	private ModularizingStorageConnector createConnectorInternal(boolean autoCommit)
			throws OntoDriverException {
		if (centralConnector == null) {
			synchronized (this) {
				if (centralConnector == null) {
					createCentralConnector();
				}
			}
		}
		final ModularizingStorageConnector conn = new ModularizingStorageConnector(centralConnector);
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector() throws OntoDriverException {
		final OwlapiStorageType type = DriverOwlapiFactory.resolveStorageType(storageProperties);
		switch (type) {
		case OWLDB:
			this.centralConnector = new OwlapiOwldbStorageConnector(storageProperties, properties);
			this.owldb = true;
			break;
		case FILE:
			this.centralConnector = new OwlapiFileStorageConnector(storageProperties, properties);
			break;
		default:
			throw new UnsupportedOperationException("Unknown storage type " + type);
		}
	}

	@Override
	public synchronized void close() throws OntoDriverException {
		if (!isOpen()) {
			return;
		}
		super.close();
		if (centralConnector != null) {
			centralConnector.close();
		}
		if (owldb) {
			OWLDBManager.getHibernateProvider().close();
		}
		ModularizingStorageConnector.reset();
	}

	@Override
	public OwlapiStatement createStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(statement, ErrorUtils.constructNPXMessage("statement"));

		return new OwlapiStatement(statement);
	}
}
