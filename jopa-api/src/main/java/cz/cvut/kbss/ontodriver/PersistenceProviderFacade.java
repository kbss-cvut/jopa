package cz.cvut.kbss.ontodriver;

import java.net.URI;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

/**
 * Facade to the persistence provider.
 * 
 * @author kidney
 * 
 */
public interface PersistenceProviderFacade {

	/**
	 * Gets metamodel of this persistence provider.
	 * 
	 * @return Metamodel
	 */
	public Metamodel getMetamodel();

	/**
	 * Retrieves entity with the specified {@code primaryKey} from the second
	 * level cache. </p>
	 * 
	 * This method may return null, since no entity with {@code primaryKey} may
	 * by present in the second level cache or the cache may be disabled.
	 * 
	 * @param cls
	 *            Type of the entity
	 * @param primaryKey
	 *            Primary key of the entity to retrieve
	 * @param context
	 *            Context URI
	 * @return Entity or null
	 * @throws NullPointerException
	 *             If {@code cls} or {@code primaryKey} is {@code null}
	 */
	public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey, URI context);
}
