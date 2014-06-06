package cz.cvut.kbss.jopa.test.utils;

import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.PersistenceUnitUtil;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.owlapi.AbstractEntityManager;
import cz.cvut.kbss.jopa.owlapi.EntityManagerFactoryImpl;
import cz.cvut.kbss.jopa.sessions.Cache;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

/**
 * This class's only purpose is to be used in {@code Metamodel} in OntoDriver
 * integration tests. It should only return properties passed to it so that
 * entity classes can be processed by the {@code Metamodel}.
 * 
 * @author kidney
 * 
 */
public class EntityManagerFactoryMock extends EntityManagerFactoryImpl {

	public EntityManagerFactoryMock() {
		super(null);
	}

	public EntityManagerFactoryMock(OntologyStorageProperties storageProperties,
			Map<String, String> properties) {
		super(storageProperties, properties);
	}

	@Override
	public void close() {
		// Do nothing
	}

	@Override
	public EntityManager createEntityManager() {
		return null;
	}

	@Override
	public EntityManager createEntityManager(Map<String, String> map) {
		return null;
	}

	@Override
	public ServerSession getServerSession() {
		return null;
	}

	@Override
	public boolean isOpen() {
		return false;
	}

	@Override
	public Set<AbstractEntityManager> getEntityManagers() {
		return null;
	}

	@Override
	public Metamodel getMetamodel() {
		return null;
	}

	@Override
	public PersistenceUnitUtil getPersistenceUnitUtil() {
		return null;
	}

	@Override
	public Object getIdentifier(Object entity) {
		return false;
	}

	@Override
	public boolean isLoaded(Object entity, String attributeName) {
		return false;
	}

	@Override
	public boolean isLoaded(Object entity) {
		return false;
	}

	@Override
	public Cache getCache() {
		return null;
	}
}
