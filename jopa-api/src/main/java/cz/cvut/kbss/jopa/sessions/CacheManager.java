package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.Set;

/**
 * This interface defines basic methods for accessing the shared live object cache.
 *
 * @author kidney
 */
public interface CacheManager extends Cache {

    /**
     * Adds the specified object into the shared session cache. </p>
     * <p>
     * If the cache already contains object with the specified primary key (and it is in the same repository context),
     * it is replaced with the one passed as argument.
     *
     * @param primaryKey Primary key of the specified object
     * @param entity     The object to be added into the cache
     * @param context    Repository context URI
     */
    public void add(Object primaryKey, Object entity, URI context);

    /**
     * Gets entity with the specified primary key from the cache. </p>
     * <p>
     * The entity is searched for in the context specified by {@code entityOrigin} . Thus all three conditions - class,
     * primary key and origin must match to return a result.
     *
     * @param cls        Class of the entity
     * @param primaryKey Primary key of the entity
     * @param context    Repository context URI
     * @return Entity with the specified primary key or {@code null}
     */
    public <T> T get(Class<T> cls, Object primaryKey, URI context);

    /**
     * Remove objects with inferred attributes from the cache, since there are changes in the ontology that might
     * influence the inferred attributes.
     */
    public void clearInferredObjects();

    /**
     * Set the inferred classes for this cache manager. </p>
     * <p>
     * Entities from inferred classes are special in that when anything in the ontology changes, they have to be evicted
     * from the cache, since they are reasoned and their attributes may change.
     *
     * @param inferredClasses Set of inferred classes
     */
    public void setInferredClasses(Set<Class<?>> inferredClasses);

    /**
     * Closes the cache.
     */
    public void close();
}
