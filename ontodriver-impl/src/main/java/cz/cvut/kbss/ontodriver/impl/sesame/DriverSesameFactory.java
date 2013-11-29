package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import cz.cvut.kbss.ontodriver.AbstractStatement;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverAbstractFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class DriverSesameFactory extends DriverAbstractFactory {

	public DriverSesameFactory(List<Context> contexts,
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
			LOG.finer("Creating Sesame storage module.");
		}
		final StorageModule m = new SesameStorageModule(ctx, persistenceProvider, this);
		registerModule(m);
		return m;
	}

	@Override
	public SesameStorageConnector createStorageConnector(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		ensureState(ctx);
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Creating Sesame storage connector.");
		}
		final OntologyStorageProperties props = contextsToProperties.get(ctx);
		assert props != null;
		final SesameStorageConnector c = new SesameStorageConnector(props, properties);
		registerConnector(c);
		return c;
	}

	@Override
	public DriverStatement createStatement(AbstractStatement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

}
