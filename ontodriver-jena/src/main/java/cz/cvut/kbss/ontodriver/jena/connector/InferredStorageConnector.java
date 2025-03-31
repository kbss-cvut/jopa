/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

public interface InferredStorageConnector extends StatementExecutor {

    /**
     * Retrieves statements corresponding to the specified criteria from the specified named graph.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     * <p>
     * {@code contexts} are also optional, their absence means that the default graph should be used.
     *
     * @param subject  Statement subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param contexts Named graph URIs, optional. Empty collection indicates the default graph should be used
     * @return Collection of matching statements, including inferred ones
     */
    Collection<Statement> findWithInference(Resource subject, Property property, RDFNode value,
                                            Collection<String> contexts);

    /**
     * Checks whether the specified context (named graph) contains any statements matching the specified criteria,
     * either asserted or inferred.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     * <p>
     * {@code context} is also optional, its absence means that the default graph should be used.
     *
     * @param subject  Subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param contexts Named graph IRIs, optional. Empty collection indicates the default graph should be used
     * @return {@code true} if at least one statement matches the criteria, {@code false} otherwise
     */
    boolean containsWithInference(Resource subject, Property property, RDFNode value, Collection<String> contexts);

    /**
     * Checks whether named graph with the specified IRI is consistent.
     *
     * @param context Named graph IRI, optional
     * @return Whether the graph is consistent
     */
    boolean isConsistent(String context);
}
