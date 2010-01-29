package cz.cvut.kbss.owlpersistence.model;

import java.util.List;

/**
 * Determine the list of persistence providers available in the runtime
 * environment.
 * 
 * Implementations must be thread-safe.
 * 
 * Note that the getPersistenceProviders method can potentially be called many
 * times: it is recommended that the implementation of this method make use of
 * caching.
 */
public interface PersistenceProviderResolver {

	/**
	 * Returns a list of the PersdistenceProvider implementations available in
	 * the runtime environment.
	 * 
	 * @return list of the persistence providers available in the environment
	 * 
	 * @see JPA 2.0
	 */
	List<PersistenceProvider> getPersistenceProviders();

	/**
	 * Clear cache of providers
	 * 
	 * @see JPA 2.0
	 */
	void clearCachedProviders();
}
