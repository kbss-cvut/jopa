package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class DriverSesameFactory extends DriverAbstractFactory {

	private SesameStorageConnectorImpl centralConnector;

	public DriverSesameFactory(OntologyStorageProperties repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositoryProperties, properties);
	}

	@Override
	public StorageModule createStorageModule(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		ensureParametersAndState(persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Sesame storage module.");
		}
		final StorageModule m = new SesameStorageModule(persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public SesameStorageConnector createStorageConnector() throws OntoDriverException {
		ensureOpen();
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Sesame storage connector.");
		}
		return createConnectorImpl();
	}

	@Override
	public DriverStatement createStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		if (statement == null) {
			throw new NullPointerException();
		}
		return new SesameStatement(statement);
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
	}

	private SesameStorageConnector createConnectorImpl() throws OntoDriverException {
		if (centralConnector == null) {
			synchronized (this) {
				if (centralConnector == null) {
					this.centralConnector = new SesameStorageConnectorImpl(storageProperties,
							properties);
				}
			}
		}
		final SesameStorageConnectorProxy c = new SesameStorageConnectorProxy(centralConnector);
		registerConnector(c);
		return c;
	}

}
