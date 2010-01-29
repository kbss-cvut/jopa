package cz.cvut.kbss.owlpersistence.owlapi;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import cz.cvut.kbss.owlpersistence.model.EntityManager;
import cz.cvut.kbss.owlpersistence.model.EntityManagerFactory;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.PersistenceUnitUtil;
import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;
import cz.cvut.kbss.owlpersistence.owlapi.fresh.EntityManagerImpl;
import cz.cvut.kbss.owlpersistence.owlapi.old.EntityManagerImplOld;

public class EntityManagerFactoryImpl implements EntityManagerFactory,
		PersistenceUnitUtil {

	private boolean open = true;

	private final Set<AbstractEntityManager> em = new HashSet<AbstractEntityManager>();
	private final Map<String, String> properties;

	private MetamodelImpl metamodel = null;

	private OWLOntologyManager m;

	public EntityManagerFactoryImpl(final Map<String, String> properties) {
		this.properties = properties;
		this.m = OWLManager.createOWLOntologyManager();
	}

	public OWLOntologyManager getOWLOntologyManager() {
		return m;
	}

	@Override
	public void close() {
		open = false;

		for (final EntityManager m : em) {
			// TODO try-catch
			m.close();
		}
	}

	@Override
	public EntityManager createEntityManager() {
		return this
				.createEntityManager(Collections.<String, String> emptyMap());
	}

	@Override
	public EntityManager createEntityManager(Map<String, String> map) {
		if (!open) {
			throw new IllegalStateException(
					"The OWLEntityManager has been closed.");
		}

		final Map<String, String> newMap = new HashMap<String, String>(map);

		newMap.putAll(properties);
		newMap.putAll(map);

//		if (!newMap
//				.containsKey(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS)) {
//			newMap.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS,
//					OWLAPIIdentityReasonerFactory.class.getName());
//		}

		String s = newMap.get(OWLAPIPersistenceProperties.USE_OLD_OWLAPIV3);

		final AbstractEntityManager c;

		if (s != null && Boolean.parseBoolean(s)) {
			c = new EntityManagerImplOld(this, newMap);
		} else {
			c = new EntityManagerImpl(this, newMap);
		}

		em.add(c);
		return c;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	public Set<AbstractEntityManager> getEntityManagers() {
		return Collections.unmodifiableSet(em);
	}

	@Override
	public Metamodel getMetamodel() {
		if (metamodel == null) {
			metamodel = new MetamodelImpl(this);
		}

		return metamodel;
	}

	@Override
	public PersistenceUnitUtil getPersistenceUnitUtil() {
		return this;
	}

	@Override
	public Object getIdentifier(Object entity) {
		try {
			return getMetamodel().entity(entity.getClass()).getIdentifier()
					.getJavaField().get(entity);
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException();
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException();
		}
	}

	@Override
	public boolean isLoaded(Object entity, String attributeName) {
		for (final AbstractEntityManager emi : em) {
			if (emi.contains(entity)) {
				if (attributeName == null) {
					return true;
				}

				return emi.isLoaded(entity, attributeName);
			}
		}

		return false;
	}

	@Override
	public boolean isLoaded(Object entity) {
		return isLoaded(entity);
	}

}
