/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.BaseEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.NonEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Allows optimizing query result loading by modifying the query assembly and providing a corresponding query result
 * loader.
 */
public class SparqlQueryResultLoadingOptimizer {

    private static final Logger LOG = LoggerFactory.getLogger(SparqlQueryResultLoadingOptimizer.class);

    private final TokenStreamSparqlQueryHolder queryHolder;

    private final UnitOfWork uow;

    private final ConnectionWrapper connection;

    private boolean optimizationEnabled;

    public SparqlQueryResultLoadingOptimizer(TokenStreamSparqlQueryHolder queryHolder, UnitOfWork uow,
                                             ConnectionWrapper connection) {
        this.queryHolder = queryHolder;
        this.uow = uow;
        this.connection = connection;
    }

    public void enableOptimization() {this.optimizationEnabled = true;}

    public void disableOptimization() {this.optimizationEnabled = false;}

    /**
     * Modifies the query assembly for optimized entity loading, if possible.
     *
     * @param resultClass Query result class
     * @param descriptor  Descriptor specified for query result loading
     * @param fetchGraph  Optional fetch graph specifying which properties should be loaded
     */
    public void optimizeQueryAssembly(Class<?> resultClass, Descriptor descriptor, EntityGraph<?> fetchGraph) {
        switch (resolveOptimizerType(resultClass, descriptor, fetchGraph)) {
            case TRIPLE_BASED:
                LOG.trace("Processing query results with triple-based optimized entity loading.");
                queryHolder.setAssemblyModifier(new UnboundPredicateObjectSparqlAssemblyModifier());
                break;
            case ATTRIBUTE_BASED:
            case FETCH_GRAPH_BASED: // Intentional fall-through
                LOG.trace("Processing query results with attribute enumeration-based optimized attribute loading.");
                queryHolder.setAssemblyModifier(new AttributeEnumeratingSparqlAssemblyModifier(uow.getMetamodel(), uow.getMetamodel()
                                                                                                                      .entity(resultClass), descriptor, fetchGraph, connection));
                break;
            default:
                // Do nothing
                break;
        }
    }

    private OptimizerType resolveOptimizerType(Class<?> resultClass, Descriptor descriptor, EntityGraph<?> fetchGraph) {
        if (!canOptimize(resultClass, descriptor, fetchGraph)) {
            return OptimizerType.NONE;
        }
        final IdentifiableEntityType<?> et = uow.getMetamodel().entity(resultClass);
        if (et.getProperties() == null && !et.hasSubtypes()) {
            return fetchGraph != null ? OptimizerType.FETCH_GRAPH_BASED : OptimizerType.ATTRIBUTE_BASED;
        }
        if (!queryContainsGraphOrServiceClause()) {
            return OptimizerType.TRIPLE_BASED;
        }
        return OptimizerType.NONE;
    }

    private boolean canOptimize(Class<?> resultClass, Descriptor descriptor, EntityGraph<?> fetchGraph) {
        return (optimizationEnabled || fetchGraph != null) && queryHolder.getQueryType() == QueryType.SELECT
                && projectsEntity(resultClass) && limitOrOffsetNotSet() && descriptorSpecifiesAtMostOneContext(descriptor);
    }

    private boolean projectsEntity(Class<?> resultClass) {
        return uow.isEntityType(resultClass) && queryHolder.getProjectedQueryParameters().size() == 1;
    }

    private boolean limitOrOffsetNotSet() {
        return !queryHolder.hasOffset() && !queryHolder.hasLimit();
    }

    /**
     * Descriptor specifies at most one repository context for the root entity and all its attributes.
     *
     * @param descriptor Descriptor to examine
     * @return {@code true} if at most one context is used, {@code false} otherwise
     */
    private boolean descriptorSpecifiesAtMostOneContext(Descriptor descriptor) {
        return descriptor.getContexts().size() <= 1 && descriptor.getAttributeDescriptors().stream()
                                                                 .allMatch(d -> d.getContexts().size() <= 1);
    }

    private boolean queryContainsGraphOrServiceClause() {
        // Do not optimize a query containing GRAPH or SERVICE with unbound predicate.
        // The optimization pattern (?x ?p ?v) has no control over individual property/attribute contexts, and it could lead to incorrect results
        return queryHolder.getQueryAttributes().hasGraphOrService();
    }

    /**
     * Gets loader of query results for the specified query.
     * <p>
     * If possible, a version supporting optimized entity loading is returned.
     *
     * @param resultClass Result class
     * @param descriptor  Descriptor specified for results
     * @param fetchGraph  Entity graph specifying which properties should be loaded
     * @param <T>         Result type
     * @return Query result loader
     */
    public <T> QueryResultLoader<T> getQueryResultLoader(Class<T> resultClass, Descriptor descriptor,
                                                         EntityGraph<T> fetchGraph) {
        final OptimizerType optimizerType = resolveOptimizerTypeForResultLoader(resultClass, descriptor, fetchGraph);
        return switch (optimizerType) {
            case TRIPLE_BASED -> new TripleBasedRowsToAxiomsQueryResultLoader<>(uow, resultClass, descriptor);
            case ATTRIBUTE_BASED, FETCH_GRAPH_BASED ->
                    new AttributeBasedRowsToAxiomsQueryResultLoader<>(uow, resultClass, descriptor, fetchGraph, resolveSubjectVariable());
            default ->
                    uow.isEntityType(resultClass) ? new BaseEntityQueryResultLoader<>(uow, resultClass, descriptor) : new NonEntityQueryResultLoader<>(resultClass);
        };
    }

    private <T> OptimizerType resolveOptimizerTypeForResultLoader(Class<T> resultClass, Descriptor descriptor,
                                                                  EntityGraph<T> fetchGraph) {
        final OptimizerType ot = resolveOptimizerType(resultClass, descriptor, fetchGraph);
        if (ot != OptimizerType.NONE) {
            return ot;
        }
        if (uow.isEntityType(resultClass) && queryHolder.getProjectedQueryParameters().size() > 1) {
            // Entity attributes are projected from the query
            return OptimizerType.ATTRIBUTE_BASED;
        }
        return ot;
    }

    private String resolveSubjectVariable() {
        assert !queryHolder.getProjectedQueryParameters().isEmpty();
        return queryHolder.getProjectedQueryParameters().get(0).getName();
    }

    private enum OptimizerType {
        TRIPLE_BASED,
        ATTRIBUTE_BASED,
        FETCH_GRAPH_BASED,
        NONE
    }
}
