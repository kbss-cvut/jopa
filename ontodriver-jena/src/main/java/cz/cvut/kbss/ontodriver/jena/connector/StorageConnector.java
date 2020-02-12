/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.List;

public interface StorageConnector extends Closeable, Wrapper, StatementExecutor {

    /**
     * Begins a transaction.
     */
    void begin();

    /**
     * Commits the current transaction.
     */
    void commit() throws JenaDriverException;

    /**
     * Rolls back the current transaction.
     */
    void rollback();

    /**
     * Retrieves statements corresponding to the specified criteria from the specified named graph.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     * <p>
     * {@code context} is also optional, its absence means that the default graph should be used.
     *
     * @param subject  Statement subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param context  Named graph IRI, optional
     * @return Collection of matching statements
     */
    Collection<Statement> find(Resource subject, Property property, RDFNode value, String context);

    /**
     * Checks whether the specified context (named graph) contains any statements matching the specified criteria.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     * <p>
     * {@code context} is also optional, its absence means that the default graph should be used.
     *
     * @param subject  Subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param context  Named graph IRI, optional
     * @return {@code true} if at least one statement matches the criteria, {@code false} otherwise
     */
    boolean contains(Resource subject, Property property, RDFNode value, String context);

    /**
     * Lists all contexts (named graph) in the repository (including the transactional ones).
     *
     * @return List of named graph URIs
     */
    List<String> getContexts();

    /**
     * Adds the specified statements to the specified context in the storage.
     * <p>
     * Requires an active transaction.
     * <p>
     * {@code context} is optional, its absence means that the statements will be added into the the default graph.
     *
     * @param statements Statements to add
     * @param context    Target context, optional
     */
    void add(List<Statement> statements, String context);

    /**
     * Removes the specified statements from the specified context in the storage.
     * <p>
     * Requires an active transaction.
     * <p>
     * {@code context} is optional, its absence means that the statements will be removed from the the default graph.
     *
     * @param statements Statements to remove
     * @param context    Target context, optional
     */
    void remove(List<Statement> statements, String context);

    /**
     * Removes statements matching the specified pattern from the specified storage context.
     * <p>
     * {@code context} is optional, its absence means that the statements will be removed from the the default graph.
     *
     * @param subject  Statement subject, optional
     * @param property Statement property, optional
     * @param object   Statement object, optional
     * @param context  Repository context IRI, optional
     */
    void remove(Resource subject, Property property, RDFNode object, String context);

    @Override
    void close() throws JenaDriverException;
}
