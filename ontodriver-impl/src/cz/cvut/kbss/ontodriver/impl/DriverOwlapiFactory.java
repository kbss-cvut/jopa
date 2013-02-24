package cz.cvut.kbss.ontodriver.impl;

import java.util.List;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.OntoDriverException;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageModule;

public class DriverOwlapiFactory extends DriverAbstractFactory {

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

	private void ensureState(Context ctx) throws OntoDriverException {
		ensureOpen();
		if (ctx == null) {
			throw new NullPointerException("Context cannot be null.");
		}
		if (!contextsToProperties.containsKey(ctx)) {
			throw new OntoDriverException("Context " + ctx + " not found.");
		}
	}
}
