package cz.cvut.kbss.ontodriver.sesame.query;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.util.StatementHolder;

import java.util.Objects;

public class SesamePreparedStatement extends SesameStatement implements PreparedStatement {

    private StatementHolder statementHolder;

    public SesamePreparedStatement(StatementExecutor executor, String statement)
            throws OntoDriverException {
        super(executor);
        this.statementHolder = new StatementHolder(statement);
        if (statementHolder.getStatement().isEmpty()) {
            throw new IllegalArgumentException("The statement string cannot be empty.");
        }
        statementHolder.analyzeStatement();
    }

    @Override
    public void setObject(String binding, Object value) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(value, ErrorUtils.constructNPXMessage("value"));
        statementHolder.setParameter(binding, value.toString());
    }

    @Override
    public ResultSet executeQuery() throws OntoDriverException {
        ensureOpen();
        return executeQuery(statementHolder.assembleStatement());
    }

    @Override
    public void executeUpdate() throws OntoDriverException {
        ensureOpen();
        executeUpdate(statementHolder.assembleStatement());
    }

    @Override
    public void clearParameters() throws OntoDriverException {
        statementHolder.clearParameters();
    }
}
