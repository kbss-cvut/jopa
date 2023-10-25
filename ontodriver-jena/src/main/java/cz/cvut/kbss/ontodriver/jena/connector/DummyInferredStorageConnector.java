/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Statement.StatementOntology;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

/**
 * This connector does not support inference, it wraps a regular {@link StorageConnector} and calls its regular methods
 * instead of performing any inference, e.g. {@link StorageConnector#contains(Resource, Property, RDFNode, Collection)}
 * for {@link #containsWithInference(Resource, Property, RDFNode, Collection)}.
 * <p>
 * Thus, inference-based methods give the same results as regular connector methods.
 */
class DummyInferredStorageConnector implements InferredStorageConnector {

    private final StorageConnector connector;

    DummyInferredStorageConnector(StorageConnector connector) {
        this.connector = connector;
    }

    @Override
    public Collection<Statement> findWithInference(Resource subject, Property property, RDFNode value,
                                                   Collection<String> contexts) {
        return connector.find(subject, property, value, contexts);
    }

    @Override
    public boolean containsWithInference(Resource subject, Property property, RDFNode value,
                                         Collection<String> context) {
        return connector.contains(subject, property, value, context);
    }

    @Override
    public boolean isConsistent(String context) {
        return true;
    }

    @Override
    public AbstractResultSet executeSelectQuery(Query query, StatementOntology target) throws JenaDriverException {
        return connector.executeSelectQuery(query, target);
    }

    @Override
    public AbstractResultSet executeAskQuery(Query query, StatementOntology target) throws JenaDriverException {
        return connector.executeAskQuery(query, target);
    }

    @Override
    public void executeUpdate(String query, StatementOntology target) throws JenaDriverException {
        connector.executeUpdate(query, target);
    }
}
