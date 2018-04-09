package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
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

    Storage(Configuration configuration) {
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

    void add(List<Statement> statements) {
        dataset.getDefaultModel().add(statements);
    }

    void add(List<Statement> statements, String context) {
        dataset.getNamedModel(context).add(statements);
    }

    void remove(List<Statement> statements) {
        dataset.getDefaultModel().remove(statements);
        if (defaultAsUnion) {
            dataset.listNames().forEachRemaining(n -> dataset.getNamedModel(n).remove(statements));
        }
    }

    void remove(StmtIterator iterator) {
        iterator.forEachRemaining(statement -> {
            dataset.getDefaultModel().remove(statement);
            if (defaultAsUnion) {
                dataset.listNames().forEachRemaining(n -> dataset.getNamedModel(n).remove(statement));
            }
        });
    }

    void remove(List<Statement> statements, String context) {
        dataset.getNamedModel(context).remove(statements);
    }

    void remove(StmtIterator iterator, String context) {
        dataset.getNamedModel(context).remove(iterator);
    }

    /**
     * Creates a storage accessor according to the specified configuration.
     *
     * @param configuration Access configuration
     * @return Storage accessor instance
     * @throws OntoDriverInitializationException When storage type is not supported
     */
    static Storage create(Configuration configuration) {
        final String type = configuration.getProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        final Storage storage;
        switch (type) {
            case JenaOntoDriverProperties.IN_MEMORY:
                storage = new MemoryStorage(configuration);
                break;
            case JenaOntoDriverProperties.FILE:
                storage = new FileStorage(configuration);
                break;
            case JenaOntoDriverProperties.SDB:
            case JenaOntoDriverProperties.TDB:
                throw new UnsupportedOperationException("Not implemented yet.");
            default:
                throw new OntoDriverInitializationException("Unsupported storage type \'" + type + "\'.");
        }
        storage.initialize();
        return storage;
    }
}
