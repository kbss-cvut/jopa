package cz.cvut.kbss.ontodriver.impl;

import java.util.List;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageModule;

public class DriverOwlapiFactory extends DriverAbstractFactory {

	static {
		try {
			OntoDriverImpl.registerFactoryClass(OntologyConnectorType.OWLAPI,
					DriverOwlapiFactory.class);
		} catch (OntoDriverException e) {
			LOG.severe("Unable to register " + DriverOwlapiFactory.class
					+ " at the driver. Message: " + e.getMessage());
		}
	}

	public DriverOwlapiFactory(List<OntologyStorageProperties> storageProperties)
			throws OntoDriverException {
		super(storageProperties);
	}

	@Override
	public StorageModule createStorageModule(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI storage module.");
		}
		final StorageModule m = new OwlapiStorageModule(ctx);
		registerModule(m);
		return m;
	}

	@Override
	public OwlapiStorageConnector createStorageConnector(Context ctx,
			boolean autoCommit) throws OntoDriverException {
		ensureState(ctx);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating OWLAPI storage connector.");
		}
		final OwlapiStorageConnector c = new OwlapiStorageConnector(
				contextsToProperties.get(ctx));
		registerConnector(c);
		return c;
	}
}
