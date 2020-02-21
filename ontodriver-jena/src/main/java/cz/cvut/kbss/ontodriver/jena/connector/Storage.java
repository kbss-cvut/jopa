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

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

abstract class Storage {

    static final Logger LOG = LoggerFactory.getLogger(Storage.class);

    private final boolean defaultAsUnion;

    Dataset dataset;

    Storage(DriverConfiguration configuration) {
        this.defaultAsUnion = configuration.is(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION);
    }

    void writeChanges() throws JenaDriverException {
        // Do nothing by default
    }

    abstract void initialize();

    Dataset getDataset() {
        return dataset;
    }

    Model getDefaultGraph() {
        return defaultAsUnion ? ModelFactory.createUnion(dataset.getUnionModel(), dataset.getDefaultModel()) :
                dataset.getDefaultModel();
    }

    Model getNamedGraph(String ctx) {
        return dataset.getNamedModel(ctx);
    }

    void begin(ReadWrite readWrite) {
        dataset.begin(readWrite);
    }

    void commit() {
        dataset.commit();
    }

    void rollback() {
        dataset.abort();
    }

    void close() {
        dataset.close();
    }

    void add(List<Statement> statements, String context) {
        if (context != null) {
            dataset.getNamedModel(context).add(statements);
        } else {
            dataset.getDefaultModel().add(statements);
        }
    }

    void remove(List<Statement> statements, String context) {
        if (context != null) {
            dataset.getNamedModel(context).remove(statements);
        } else {
            dataset.getDefaultModel().remove(statements);
            if (defaultAsUnion) {
                dataset.listNames().forEachRemaining(n -> dataset.getNamedModel(n).remove(statements));
            }
        }
    }

    void remove(StmtIterator iterator, String context) {
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

    void reload() {
        // Do nothing by default
    }

    /**
     * Sets the dataset on this storage.
     *
     * Note that by default this method throws {@link UnsupportedOperationException}, because such an operation is supported
     * only by the in-memory storage.
     * @param dataset The new dataset
     */
    void setDataset(Dataset dataset) {
        throw new UnsupportedOperationException("Cannot set dataset on storage of type " + getClass().getSimpleName());
    }

    /**
     * Creates a storage accessor according to the specified configuration.
     *
     * @param configuration Access configuration
     * @return Storage accessor instance
     * @throws OntoDriverInitializationException When storage type is not supported
     */
    static Storage create(DriverConfiguration configuration) {
        final String type = configuration.getProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        final Storage storage;
        switch (type) {
            case JenaOntoDriverProperties.IN_MEMORY:
                storage = new MemoryStorage(configuration);
                break;
            case JenaOntoDriverProperties.FILE:
                storage = new FileStorage(configuration);
                break;
            case JenaOntoDriverProperties.TDB:
                storage = new TDBStorage(configuration);
                break;
            case JenaOntoDriverProperties.SDB:
                throw new UnsupportedOperationException("Not implemented, yet.");
            default:
                throw new OntoDriverInitializationException("Unsupported storage type \'" + type + "\'.");
        }
        storage.initialize();
        return storage;
    }
}
