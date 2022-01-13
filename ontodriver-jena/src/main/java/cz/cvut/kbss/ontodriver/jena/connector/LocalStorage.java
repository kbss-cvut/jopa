/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.sparql.core.Transactional;
import org.apache.jena.update.UpdateAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Represents a local Jena storage, e.g., a TDB dataset.
 */
abstract class LocalStorage implements Storage {

    static final Logger LOG = LoggerFactory.getLogger(LocalStorage.class);

    private final boolean defaultAsUnion;

    Dataset dataset;

    LocalStorage(DriverConfiguration configuration) {
        this.defaultAsUnion = configuration.is(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION);
    }

    public void writeChanges() throws JenaDriverException {
        // Do nothing by default
    }

    public Dataset getDataset() {
        return dataset;
    }

    public Model getDefaultGraph() {
        return defaultAsUnion ? ModelFactory.createUnion(dataset.getUnionModel(), dataset.getDefaultModel()) :
               dataset.getDefaultModel();
    }

    public Model getNamedGraph(String ctx) {
        return dataset.getNamedModel(ctx);
    }

    @Override
    public Transactional getTransactional() {
        return dataset;
    }

    public void begin(ReadWrite readWrite) {
        dataset.begin(readWrite);
    }

    public void commit() {
        dataset.commit();
    }

    public void rollback() {
        dataset.abort();
    }

    public void close() {
        dataset.close();
    }

    public void add(List<Statement> statements, String context) {
        if (context != null) {
            dataset.getNamedModel(context).add(statements);
        } else {
            dataset.getDefaultModel().add(statements);
        }
    }

    public void remove(List<Statement> statements, String context) {
        if (context != null) {
            dataset.getNamedModel(context).remove(statements);
        } else {
            dataset.getDefaultModel().remove(statements);
            if (defaultAsUnion) {
                dataset.listNames().forEachRemaining(n -> dataset.getNamedModel(n).remove(statements));
            }
        }
    }

    public void remove(StmtIterator iterator, String context) {
        if (context != null) {
            dataset.getNamedModel(context).remove(iterator);
        } else {
            iterator.forEachRemaining(statement -> {
                dataset.getDefaultModel().remove(statement);
                if (defaultAsUnion) {
                    dataset.listNames().forEachRemaining(n -> dataset.getNamedModel(n).remove(statement));
                }
            });
        }
    }

    @Override
    public QueryExecution prepareQuery(Query query) {
        return QueryExecutionFactory.create(query, dataset);
    }

    @Override
    public void executeUpdate(String update) {
        UpdateAction.parseExecute(update, dataset);
    }
}
