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

import cz.cvut.kbss.jopa.model.QueryResultLoader;

import java.util.function.Supplier;

/**
 * A strategy that combines a SPARQL query assembly modification with a corresponding query result loader.
 * <p>
 * Each strategy expresses a single way of optimizing query result loading: it knows how to rewrite the SPARQL query
 * (if at all) and how to decode the resulting rows into instances of the requested type. Pairing the two halves into
 * a single object removes the need for a separate visitor double-dispatch between modifiers and loaders.
 *
 * @param assemblyModifier Modifier applied to the SPARQL query during assembly, or {@code null} if no rewrite is needed
 * @param loaderFactory    Factory creating a fresh {@link QueryResultLoader} bound to this strategy
 * @param <T>              Result type
 */
public record QueryResultLoadingStrategy<T>(SparqlAssemblyModifier assemblyModifier,
                                            Supplier<QueryResultLoader<T>> loaderFactory) {

    /**
     * Creates a new {@link QueryResultLoader} for this strategy.
     *
     * @return Query result loader
     */
    public QueryResultLoader<T> createLoader() {
        return loaderFactory.get();
    }
}
