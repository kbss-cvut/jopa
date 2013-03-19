package cz.cvut.kbss.ontodriver.impl.jena;

import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.OntoDriverImpl;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverOwlapiFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiBasedJenaModule;

public class DriverJenaFactory extends DriverAbstractFactory {

	static {
		try {
			OntoDriverImpl
					.registerFactoryClass(OntologyConnectorType.JENA, DriverJenaFactory.class);
		} catch (OntoDriverException e) {
			LOG.severe("Unable to register " + DriverOwlapiFactory.class
					+ " at the driver. Message: " + e.getMessage());
		}
	}

	public DriverJenaFactory(List<Context> contexts,
			Map<Context, OntologyStorageProperties> ctxsToProperties, Map<String, String> properties)
			throws OntoDriverException {
		super(contexts, ctxsToProperties, properties);
	}

	@Override
	public StorageModule createStorageModule(Context ctx,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx, persistenceProvider);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Jena storage module.");
		}
		final StorageModule m = new OwlapiBasedJenaModule(ctx, persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public JenaFileStorageConnector createStorageConnector(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Jena storage connector.");
		}
		final JenaFileStorageConnector c = new JenaFileStorageConnector(contextsToProperties.get(ctx),
				properties);
		registerConnector(c);
		return c;
	}
}
