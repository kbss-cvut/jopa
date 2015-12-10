package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.util.StatementHolder;

import java.util.Objects;

public class OwlapiPreparedStatement extends OwlapiStatement implements PreparedStatement {

    private final StatementHolder statementHolder;

    public OwlapiPreparedStatement(StatementExecutorFactory executorFactory, OwlapiConnection connection,
                                   String statement) {
        super(executorFactory, connection);
        this.statementHolder = new StatementHolder(statement);
        if (statementHolder.getStatement().isEmpty()) {
            throw new IllegalArgumentException("Statement cannot be empty.");
        }
        statementHolder.analyzeStatement();
    }

    @Override
    public ResultSet executeQuery() throws OntoDriverException {
        ensureOpen();
        return getExecutor().executeQuery(statementHolder.assembleStatement(), this);
    }

    @Override
    public void executeUpdate() throws OntoDriverException {
        ensureOpen();
        getExecutor().executeUpdate(statementHolder.assembleStatement());
        connection.commitIfAuto();
    }

    @Override
    public void setObject(String binding, Object value) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(binding, ErrorUtils.constructNPXMessage("binding"));
        Objects.requireNonNull(value, ErrorUtils.constructNPXMessage("value"));
        statementHolder.setParameter(binding, value.toString());
    }

    @Override
    public void clearParameters() throws OntoDriverException {
        ensureOpen();
        statementHolder.clearParameters();
    }
}
