package cz.cvut.kbss.owlpersistence;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.owlpersistence.model.EntityManagerFactory;
import cz.cvut.kbss.owlpersistence.model.LoadState;
import cz.cvut.kbss.owlpersistence.model.PersistenceProperties;
import cz.cvut.kbss.owlpersistence.model.PersistenceProvider;
import cz.cvut.kbss.owlpersistence.model.PersistenceProviderResolverHolder;
import cz.cvut.kbss.owlpersistence.model.PersistenceUtil;

public class Persistence {

	private static final Logger LOG = Logger.getLogger(Persistence.class
			.getName());

	private static final Map<String, Map<String, String>> mapProp = new HashMap<String, Map<String, String>>();

	private static final Set<PersistenceProvider> map = new HashSet<PersistenceProvider>();

	private static final PersistenceUtil pu = new PersistenceUtilImpl();

	static {
		// TODO load persistence providers
	}

	/**
	 * Create and return an EntityManagerFactory for the named persistence unit.
	 * 
	 * @param persistenceUnitName
	 *            the name of the persistence unit
	 * @return the factory that creates EntityManagers configured according to
	 *         the specified persistence unit.
	 */
	public static EntityManagerFactory createEntityManagerFactory(
			final String persistenceUnitName) {
		return createEntityManagerFactory(persistenceUnitName, Collections
				.<String, String> emptyMap());
	}

	public static EntityManagerFactory createEntityManagerFactory(
			final String persistenceUnitName,
			final Map<String, String> parameters) {

		// if (map.containsKey(persistenceUnitName)) {
		// throw new IllegalArgumentException("Persistent unit of the name '"
		// + persistenceUnitName + "' already configured.");
		// }

		final Map<String, String> realParams = new HashMap<String, String>();

		if (mapProp.containsKey(persistenceUnitName)) {
			realParams.putAll(mapProp.get(persistenceUnitName));
		}
		realParams.putAll(parameters);

		final String className = realParams
				.get(PersistenceProperties.JPA_PERSISTENCE_PROVIDER);

		if (className == null) {
			throw new IllegalArgumentException(
					"Persistent unit provider unknown.");
		}

		try {
			final PersistenceProvider pp = ((Class<PersistenceProvider>) Class
					.forName(className)).newInstance();

			// TODO get at runtime
			map.add(pp);

			return pp.createEntityManagerFactory(persistenceUnitName,
					realParams);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
			throw new IllegalArgumentException(
					"Problems with creating EntityManagerFactory.");
		}
	}

	/**
	 * @return PersistenceUtil instance
	 */
	public static PersistenceUtil getPersistenceUtil() {
		return pu;
	}

	private static PersistenceProvider getProvider(final Object o) {
		for (final PersistenceProvider emp : map) {
			if (!emp.getProviderUtil().isLoaded(o).equals(LoadState.UNKNOWN)) {
				return emp;
			}
		}
		return null;
	}
}

class PersistenceUtilImpl implements PersistenceUtil {

	public boolean isLoaded(Object entity, String attributeName) {
		for (final PersistenceProvider pp : PersistenceProviderResolverHolder
				.getPersistenceProviderResolver().getPersistenceProviders()) {

			switch (pp.getProviderUtil().isLoadedWithoutReference(entity,
					attributeName)) {
			case LOADED:
				return true;
			case NOT_LOADED:
				return false;
			default:
				;
			}
		}

		for (final PersistenceProvider pp : PersistenceProviderResolverHolder
				.getPersistenceProviderResolver().getPersistenceProviders()) {

			switch (pp.getProviderUtil().isLoadedWithReference(entity,
					attributeName)) {
			case LOADED:
				return true;
			case NOT_LOADED:
				return false;
			default:
				;
			}
		}
		return true;
	}

	public boolean isLoaded(Object entity) {
		for (final PersistenceProvider pp : PersistenceProviderResolverHolder
				.getPersistenceProviderResolver().getPersistenceProviders()) {

			switch (pp.getProviderUtil().isLoaded(entity)) {
			case LOADED:
				return true;
			case NOT_LOADED:
				return false;
			default:
				;
			}
		}

		return true;
	}

}
