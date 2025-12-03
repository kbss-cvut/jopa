package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.query.QueryHolder;

/**
 * Allows optimizing query results by modifying the query assembly and providing a corresponding query result loader.
 *
 * @param <H> Query holder type
 */
public abstract class QueryResultLoadingOptimizer<H extends QueryHolder> {

    protected final H queryHolder;

    protected boolean optimizationEnabled;

    protected QueryResultLoadingOptimizer(H queryHolder) {this.queryHolder = queryHolder;}

    public void enableOptimization() {this.optimizationEnabled = true;}

    public void disableOptimization() {this.optimizationEnabled = false;}

    /**
     * Modifies the query assembly for optimized entity loading, if possible.
     *
     * @param resultClass Query result class
     * @param descriptor  Descriptor specified for query result loading
     */
    public abstract void optimizeQueryAssembly(Class<?> resultClass, Descriptor descriptor);

    /**
     * Gets loader of query results for the specified query.
     * <p>
     * If possible, a version supporting optimized entity loading is returned (depending on whether
     * {@link #optimizeQueryAssembly(Class, Descriptor)} has been called and the assembled query optimized).
     *
     * @param resultClass Query result class
     * @param descriptor  Descriptor specified for results
     * @param <T>         Result type
     * @return Query result loader
     */
    public abstract <T> QueryResultLoader<T> getQueryResultLoader(Class<T> resultClass, Descriptor descriptor);
}
