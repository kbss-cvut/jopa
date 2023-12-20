/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query;

/**
 * Defines query hints supported by JOPA.
 */
public class QueryHints {

    /**
     * Indicates that inferred results should be omitted from query results.
     */
    public static final String DISABLE_INFERENCE = "cz.cvut.kbss.jopa.query.disableInference";

    /**
     * Allows to specify the target ontology for query execution.
     * <p>
     * By target ontology, it is meant either the shared ontology, which does not contain pending transactional changes,
     * or the transactional ontology (w.r.t. the persistence context issuing the query), where transactional changes may
     * influence the query results.
     *
     * Note that OntoDriver implementations may choose to ignore the selection depending on their internal transaction
     * and query execution mechanism.
     *
     * Valid values are {@literal CENTRAL} and {@literal TRANSACTIONAL}.
     */
    public static final String TARGET_ONTOLOGY = "cz.cvut.kbss.jopa.query.targetOntology";

    private QueryHints() {
        throw new AssertionError();
    }
}
