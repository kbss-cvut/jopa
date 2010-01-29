package cz.cvut.kbss.owlpersistence.model;

import java.util.Map;

/**
 * @see JPA 2.0
 */
public interface PersistenceProvider {

	/**
	 * Called by Persistence class when an EntityManagerFactory is to be
	 * created.
	 * 
	 * @param emName
	 *            the name of the persistence unit
	 * @param map
	 *            a Map of properties for use by the persistence provider. These
	 *            properties may be used to override the values of the
	 *            corresponding elements in the persistence.xml file or specify
	 *            values for properties not specified in the persistence.xml
	 *            (and may be null if no properties are specified).
	 * @return EntityManagerFactory for the persistence unit, or null if the
	 *         provider is not the right provider.
	 */
	public EntityManagerFactory createEntityManagerFactory(String emName,
			Map map);

	// TODO JPA 2.0
	// public EntityManagerFactory
	// createContainerEntityManagerFactory(PersistenceUnitInfo info, Map map);

	/**
	 * Return the utility interface implemented by the persistence provider.
	 * 
	 * @return ProviderUtil interface
	 */
	public ProviderUtil getProviderUtil();
}
