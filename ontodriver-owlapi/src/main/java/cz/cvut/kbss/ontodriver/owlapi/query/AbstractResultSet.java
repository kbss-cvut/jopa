package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.owl2query.model.QueryResult;
import org.semanticweb.owlapi.model.OWLObject;

public abstract class AbstractResultSet implements ResultSet {

    private boolean open;
    private final Statement statement;

    protected AbstractResultSet(Statement statement) {
        this.statement = statement;
        this.open = true;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The result set is closed.");
        }
    }

    @Override
    public cz.cvut.kbss.ontodriver.Statement getStatement() throws OntoDriverException {
        return statement;
    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    public static ResultSet createResultSet(QueryResult<OWLObject> result, Statement statement, String query) {
        if (isAskQuery(query)) {
            return new AskResultSet(result, statement);
        } else {
            return new SelectResultSet(result, statement);
        }
    }

    private static boolean isAskQuery(String statement) {
        return statement.toLowerCase().startsWith("ask");
    }
}
