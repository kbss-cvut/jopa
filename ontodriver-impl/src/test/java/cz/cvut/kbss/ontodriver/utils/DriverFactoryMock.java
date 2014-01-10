package cz.cvut.kbss.ontodriver.utils;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.DriverStatement;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class DriverFactoryMock implements DriverFactory {

	private static List<Context> contexts;
	private static Map<Context, StorageModuleMock> defaultModules;

	private boolean open = true;
	private final Map<Context, StorageModuleMock> modules;

	public DriverFactoryMock() {
		this.modules = new HashMap<Context, StorageModuleMock>();
	}

	public DriverFactoryMock(List<Context> contexts,
			Map<Context, OntologyStorageProperties> ctxsToPros, Map<String, String> properties) {
		this.modules = new HashMap<Context, StorageModuleMock>();
	}

	@Override
	public void close() throws OntoDriverException {
		open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public List<Context> getContexts() {
		return contexts;
	}

	@Override
	public StorageModule createStorageModule(Context ctx,
			PersistenceProviderFacade persistenceProvider, boolean autoCommit)
			throws OntoDriverException {
		final StorageModuleMock m = defaultModules.get(ctx);
		modules.put(ctx, m);
		return m;
	}

	@Override
	public void releaseStorageModule(StorageModule module) throws OntoDriverException {
		modules.remove(module.getContext());
		module.close();
	}

	@Override
	public StorageConnector createStorageConnector(Context ctx, boolean autoCommit)
			throws OntoDriverException {
		// Do nothing
		return null;
	}

	@Override
	public void releaseStorageConnector(StorageConnector connector) throws OntoDriverException {
		// Do nothing
	}

	public Map<Context, StorageModuleMock> getModules() {
		return modules;
	}

	public static Map<Context, StorageModuleMock> getDefaultModules() {
		return defaultModules;
	}

	public void reset() {
		init(this);
	}

	public static void init(DriverFactoryMock instance) {
		contexts = initContexts();
		defaultModules = initModules(instance);
	}

	private static List<Context> initContexts() {
		final List<Context> list = new ArrayList<Context>();
		list.add(new Context(URI.create("http://contextOne"), OntologyConnectorType.OWLAPI));
		list.add(new Context(URI.create("http://contextTwo"), OntologyConnectorType.JENA));
		return list;
	}

	private static Map<Context, StorageModuleMock> initModules(DriverFactoryMock instance) {
		final Map<Context, StorageModuleMock> map = new HashMap<Context, StorageModuleMock>();
		try {
			map.put(contexts.get(0), new StorageModuleMock(contexts.get(0),
					new PersistenceProviderMock(), instance));
			map.put(contexts.get(1), new StorageModuleMock(contexts.get(1),
					new PersistenceProviderMock(), instance));
		} catch (OntoDriverException e) {
			// Do nothing
		}
		return map;
	}

	@Override
	public DriverStatement createStatement(JopaStatement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}
}
