package cz.cvut.kbss.ontodriver.utils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class StorageManagerMock extends StorageManager {

	public static URI CONTEXT_ONE_URI = URI.create("http://jopa-context-one");
	public static URI CONTEXT_TWO_URI = URI.create("http://jopa-context-two");
	public static URI CONTEXT_THREE_URI = URI.create("http://jopa-context-three");

	private final Map<URI, Context> contexts;
	private final Map<Context, Map<Object, Object>> entities;

	private boolean committed;
	private boolean rolledBack;

	public StorageManagerMock(PersistenceProviderFacade persistenceProvider) {
		super(persistenceProvider);
		this.contexts = initContexts();
		this.entities = initEntities();
		this.committed = false;
		this.rolledBack = false;
	}

	@Override
	public void commit() throws OntoDriverException {
		this.committed = true;
	}

	@Override
	public void rollback() throws OntoDriverException {
		this.rolledBack = true;
	}

	@Override
	public ResultSet executeStatement(Statement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean contains(Object primaryKey, Context entityContext) throws OntoDriverException {
		assert primaryKey != null;
		assert entityContext != null;
		final Map<Object, Object> m = entities.get(entityContext);
		if (m != null) {
			return m.containsKey(primaryKey);
		}
		return false;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, Context entityContext,
			Map<String, Context> attributeContexts) throws OntoDriverException {
		assert cls != null;
		assert primaryKey != null;
		assert entityContext != null;
		final Map<Object, Object> m = entities.get(entityContext);
		if (m != null) {
			final Object o = m.get(primaryKey);
			if (o != null) {
				if (!cls.isAssignableFrom(o.getClass())) {
					throw new OntoDriverException();
				}
				return cls.cast(o);
			}
		}
		return null;
	}

	@Override
	public List<Context> getAvailableContexts() {
		return new ArrayList<Context>(contexts.values());
	}

	@Override
	public Map<URI, Context> getContextsByUris() {
		return Collections.unmodifiableMap(contexts);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, Context context)
			throws OntoDriverException {
		// Do nothing
	}

	@Override
	public <T> void merge(Object primaryKey, T entity, Context entityContext,
			Map<String, Context> attributeContexts) throws OntoDriverException {
		assert primaryKey != null;
		assert entity != null;
		assert entityContext != null;
		final Map<Object, Object> m = entities.get(entityContext);
		assert m != null;
		m.put(primaryKey, entity);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, Context entityContext,
			Map<String, Context> attributeContexts) throws OntoDriverException {
		assert primaryKey != null;
		assert entity != null;
		assert entityContext != null;
		final Map<Object, Object> m = entities.get(entityContext);
		assert m != null;
		m.put(primaryKey, entity);
	}

	@Override
	public void remove(Object primaryKey, Context entityContext) throws OntoDriverException {
		assert primaryKey != null;
		assert entityContext != null;
		final Map<Object, Object> m = entities.get(entityContext);
		if (m != null) {
			m.remove(primaryKey);
		}
	}

	public Map<Context, Map<Object, Object>> getEntities() {
		return entities;
	}

	public boolean isCommitted() {
		return committed;
	}

	public boolean isRolledBack() {
		return rolledBack;
	}

	private Map<URI, Context> initContexts() {
		final Map<URI, Context> m = new HashMap<URI, Context>();
		final Context cOne = new Context(CONTEXT_ONE_URI, OntologyConnectorType.OWLAPI);
		m.put(cOne.getUri(), cOne);
		final Context cTwo = new Context(CONTEXT_TWO_URI, OntologyConnectorType.JENA);
		m.put(cTwo.getUri(), cTwo);
		final Context cThree = new Context(CONTEXT_THREE_URI, OntologyConnectorType.OWLAPI);
		m.put(cThree.getUri(), cThree);
		return m;
	}

	private Map<Context, Map<Object, Object>> initEntities() {
		final Map<Context, Map<Object, Object>> map = new HashMap<Context, Map<Object, Object>>();
		final Map<Object, Object> cOneMap = new HashMap<Object, Object>();
		map.put(contexts.get(CONTEXT_ONE_URI), cOneMap);
		final OWLClassA aOne = new OWLClassA();
		aOne.setUri(URI.create("http://testAOne"));
		aOne.setStringAttribute("FirstOne");
		cOneMap.put(aOne.getUri(), aOne);
		final OWLClassB bOne = new OWLClassB();
		bOne.setUri(URI.create("http://testBOne"));
		bOne.setStringAttribute("SecondOne");
		cOneMap.put(bOne.getUri(), bOne);
		final Map<Object, Object> cTwoMap = new HashMap<Object, Object>();
		map.put(contexts.get(CONTEXT_TWO_URI), cTwoMap);
		final OWLClassA aTwo = new OWLClassA();
		aTwo.setUri(URI.create("http://testATwo"));
		aTwo.setStringAttribute("ThirdOne");
		aTwo.setTypes(new HashSet<String>());
		aTwo.getTypes().add("TypeOne");
		aTwo.getTypes().add("TypeThree");
		cTwoMap.put(aTwo.getUri(), aTwo);
		final OWLClassD dOne = new OWLClassD();
		dOne.setUri(URI.create("http://testDOne"));
		dOne.setOwlClassA(aTwo);
		cTwoMap.put(dOne.getUri(), dOne);
		final Map<Object, Object> cThreeMap = new HashMap<Object, Object>();
		map.put(contexts.get(CONTEXT_THREE_URI), cThreeMap);
		for (int i = 0; i < 10; i++) {
			final OWLClassB b = new OWLClassB();
			b.setUri(URI.create("http://testB" + i));
			b.setStringAttribute("StringAttribute" + i);
			cThreeMap.put(b.getUri(), b);
		}
		return map;
	}
}
