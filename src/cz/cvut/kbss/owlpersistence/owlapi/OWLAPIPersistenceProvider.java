package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.EntityManagerFactory;
import cz.cvut.kbss.owlpersistence.model.LoadState;
import cz.cvut.kbss.owlpersistence.model.PersistenceProvider;
import cz.cvut.kbss.owlpersistence.model.ProviderUtil;

public class OWLAPIPersistenceProvider implements PersistenceProvider,
		ProviderUtil {

	private static Set<EntityManagerFactoryImpl> emfs = new HashSet<EntityManagerFactoryImpl>();

	public OWLAPIPersistenceProvider() {
	}

	@Override
	public EntityManagerFactory createEntityManagerFactory(String emName,
			Map map) {
		final EntityManagerFactoryImpl emf = new EntityManagerFactoryImpl(map);
		emfs.add(emf);
		return emf;
	}

	@Override
	public ProviderUtil getProviderUtil() {
		return this;
	}

	@Override
	public LoadState isLoaded(Object entity) {
		return LoadState.UNKNOWN;
	}

	@Override
	public LoadState isLoadedWithReference(Object entity, String attributeName) {
		return LoadState.UNKNOWN;
	}

	@Override
	public LoadState isLoadedWithoutReference(Object entity,
			String attributeName) {
		return LoadState.UNKNOWN;
	}

	private static AbstractEntityManager find(Object o) {
		for (final EntityManagerFactoryImpl emfi : emfs) {
			for (final AbstractEntityManager emi : emfi.getEntityManagers()) {
				if (emi.isOpen() && emi.contains(o)) {
					return emi;
				}
			}
		}

		return null;
	}

	static void loadReference(Object o, Field f) {
		final AbstractEntityManager ei = find(o);

		if (ei != null) {
			ei.loadReference(o, f);
		}
	}

	static void saveReference(Object o, Field f) {
		final AbstractEntityManager ei = find(o);

		if (ei != null) {
			ei.saveReference(o, f);
		}
	}
}
