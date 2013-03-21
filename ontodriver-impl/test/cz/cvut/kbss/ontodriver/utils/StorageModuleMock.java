package cz.cvut.kbss.ontodriver.utils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class StorageModuleMock extends StorageModule {

	private static final Map<Object, Object> defaultEntities = initEntities();

	private Map<Object, Object> entities;
	private List<Object> uris;

	private boolean commit = false;

	public StorageModuleMock(Context context, PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		super(context, persistenceProvider, factory);
	}

	@Override
	public void commit() throws OntoDriverException {
		this.commit = true;
	}

	@Override
	public void rollback() throws OntoDriverException {
		resetEntities();
	}

	@Override
	protected void initialize() throws OntoDriverException {
		resetEntities();
	}

	@Override
	public boolean contains(Object primaryKey) throws OntoDriverException {
		return entities.containsKey(primaryKey);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		final Object o = entities.get(primaryKey);
		return cls.cast(o);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		// do nothing
	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		entities.put(primaryKey, entity);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		entities.put(primaryKey, entity);
	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		entities.remove(primaryKey);
	}

	@Override
	public ResultSet executeStatement(Statement statement) throws OntoDriverException {
		// not implemented yet
		return null;
	}

	private static Map<Object, Object> initEntities() {
		final Map<Object, Object> map = new HashMap<Object, Object>();
		final OWLClassB bOne = new OWLClassB();
		bOne.setUri(URI.create("http://bOne"));
		bOne.setStringAttribute("bOneStringAttribute");
		map.put(bOne.getUri(), bOne);
		final OWLClassA aOne = new OWLClassA();
		aOne.setUri(URI.create("http://aOne"));
		aOne.setStringAttribute("aOneStringAttribute");
		final String[] aTypes = { "TOne", "TTwo", "TThree" };
		aOne.setTypes(new HashSet<String>(Arrays.asList(aTypes)));
		map.put(aOne.getUri(), aOne);
		final OWLClassB bTwo = new OWLClassB();
		bTwo.setUri(URI.create("http://bTwo"));
		bTwo.setStringAttribute("bTwoStringAttribute");
		map.put(bTwo.getUri(), bTwo);
		final OWLClassD dOne = new OWLClassD();
		dOne.setUri(URI.create("http://dOne"));
		dOne.setOwlClassA(aOne);
		map.put(dOne.getUri(), dOne);
		return map;
	}

	public void resetEntities() {
		this.entities = new HashMap<Object, Object>(defaultEntities);
		this.uris = new ArrayList<Object>(defaultEntities.keySet());
		this.commit = false;
	}

	public Map<Object, Object> getEntities() {
		return entities;
	}

	public List<Object> getUris() {
		return uris;
	}

	public boolean isCommited() {
		return commit;
	}
}
