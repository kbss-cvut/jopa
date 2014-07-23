package cz.cvut.kbss.ontodriver.impl.owlim;

import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwlimOntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.jena.JenaCachingStorageConnector;
import cz.cvut.kbss.ontodriver.impl.jena.OwlapiBasedJenaConnector;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiBasedCachingJenaModule;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStatement;

/**
 * Factory for OWLIM storage connectors. </p>
 * 
 * The current version uses OWLIM's Jena connector and supports only local
 * repositories.
 * 
 * @author kidney
 * 
 */
public class DriverOwlimFactory extends DriverAbstractFactory {

	private OwlapiBasedJenaConnector centralConnector;

	public DriverOwlimFactory(OntologyStorageProperties repositoryProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(repositoryProperties, properties);
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

	@Override
	public StorageModule createStorageModule(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException {
		ensureParametersAndState(persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating caching Jena storage module.");
		}
		final StorageModule m = new OwlapiBasedCachingJenaModule(persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public StorageConnector createStorageConnector() throws OntoDriverException {
		ensureOpen();
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLIM storage connector.");
		}
		return createConnectorInternal();
	}

	private synchronized OwlapiBasedJenaConnector createConnectorInternal()
			throws OntoDriverException {
		if (centralConnector == null) {
			synchronized (this) {
				if (centralConnector == null) {
					createCentralConnector();
				}
			}
		}
		final JenaCachingStorageConnector conn = new JenaCachingStorageConnector(centralConnector);
		registerConnector(conn);
		return conn;
	}

	private void createCentralConnector() throws OntoDriverException {
		if (!(storageProperties instanceof OwlimOntologyStorageProperties)) {
			throw new OntoDriverException(
					"The storage properties is not suitable for OWLIM based connectors.");
		}
		final OwlimOntologyStorageProperties owlimP = (OwlimOntologyStorageProperties) storageProperties;
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Creating central OWLIM storage connector.");
		}
		this.centralConnector = new JenaBasedOwlimConnector(owlimP, properties);
	}

	@Override
	public DriverStatement createStatement(JopaStatement statement) throws OntoDriverException {
		if (statement == null) {
			throw new NullPointerException();
		}
		return new OwlapiStatement(statement);
	}
}
