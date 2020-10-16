package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFuseki;
import org.apache.jena.sparql.core.Transactional;

import java.util.List;

/**
 * Represents a connection to a Jena Fuseki server.
 */
class FusekiStorage implements Storage {

    private final boolean defaultAsUnion;
    private final String serverUrl;

    private RDFConnection connection;

    FusekiStorage(DriverConfiguration configuration) {
        this.defaultAsUnion = configuration.is(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION);
        this.serverUrl = configuration.getStorageProperties().getPhysicalURI().toString();
    }

    private RDFConnection connect() {
        if (connection == null) {
            this.connection = RDFConnectionFuseki.create().destination(serverUrl).build();
        }
        return connection;
    }

    @Override
    public Transactional getTransactional() {
        return connect();
    }

    @Override
    public Dataset getDataset() {
        return connect().fetchDataset();
    }

    @Override
    public Model getDefaultGraph() {
        if (defaultAsUnion) {
            return ModelFactory.createUnion(connect().fetch(), getDataset().getUnionModel());
        } else {
            return connect().fetch();
        }
    }

    @Override
    public Model getNamedGraph(String ctx) {
        return connect().fetch(ctx);
    }

    @Override
    public void begin(ReadWrite readWrite) {
        connect().begin(readWrite);
    }

    @Override
    public void commit() {
        connection.commit();
        closeConnection();
    }

    @Override
    public void rollback() {
        connection.abort();
        closeConnection();
    }

    @Override
    public void close() {
        closeConnection();
    }

    private void closeConnection() {
        if (connection != null) {
            connection.close();
            this.connection = null;
        }
    }

    @Override
    public void add(List<Statement> statements, String context) {
        assert connection != null && connection.isInTransaction();
        if (statements.isEmpty()) {
            return;
        }
        final Model model = ModelFactory.createDefaultModel().add(statements);
        if (context != null) {
            connection.load(context, model);
        } else {
            connection.load(model);
        }
    }

    @Override
    public void remove(List<Statement> statements, String context) {
        assert connection != null && connection.isInTransaction();
        if (statements.isEmpty()) {
            return;
        }
        // Note that given the way Fuseki connection works, this can be quite inefficient (fetch model, update it, upload it again)
        // So translation to a SPARQL update may be more appropriate
        if (context != null) {
            final Model m = connection.fetch(context);
            m.remove(statements);
            connection.put(context, m);
        } else {
            final Model def = connection.fetch();
            def.remove(statements);
            connection.put(def);
            if (defaultAsUnion) {
                connection.querySelect("SELECT ?g WHERE { ?g {} }", qs -> {
                    final String ctx = qs.getResource("g").getURI();
                    final Model m = connect().fetch(ctx);
                    m.remove(statements);
                    connection.put(ctx, m);
                });
            }
        }
    }

    @Override
    public void remove(StmtIterator iterator, String context) {
        assert connection != null && connection.isInTransaction();
        final List<Statement> statements = iterator.toList();
        remove(statements, context);
    }

    @Override
    public QueryExecution prepareQuery(Query query) {
        return connect().query(query);
    }

    @Override
    public void executeUpdate(String update) {
        connect().update(update);
    }
}
