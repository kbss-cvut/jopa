package cz.cvut.kbss.ontodriver.jena.query;


import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.RDFNode;

import java.util.NoSuchElementException;
import java.util.Observer;

public class SelectResultSet implements cz.cvut.kbss.ontodriver.ResultSet {

    private boolean open = true;

    private JenaStatement statement;

    private final QueryExecution execution;
    private final ResultSet jenaResult;
    private QuerySolution current;
    private int rowIndex = -1;

    public SelectResultSet(QueryExecution execution, ResultSet jenaResult) {
        this.execution = execution;
        this.jenaResult = jenaResult;
    }

    void setStatement(JenaStatement statement) {
        this.statement = statement;
    }

    @Override
    public Statement getStatement() {
        ensureOpen();
        assert statement != null;
        return statement;
    }

    @Override
    public int findColumn(String variableName) {
        ensureOpen();
        return jenaResult.getResultVars().indexOf(variableName);
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This result set is closed.");
        }
    }

    @Override
    public int getColumnCount() {
        ensureOpen();
        return jenaResult.getResultVars().size();
    }

    @Override
    public void first() {
        throw new UnsupportedOperationException("This result set does not support returning to the first rowIndex.");
    }

    @Override
    public boolean getBoolean(int variableIndex) throws JenaDriverException {
        ensureState();
        return getBoolean(getVariableAt(variableIndex));
    }

    private void ensureState() {
        ensureOpen();
        if (current == null) {
            throw new IllegalStateException("Must call next before getting any values.");
        }
    }

    @Override
    public boolean getBoolean(String variableName) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return getLiteral(variableName).getBoolean();
    }

    private Literal getLiteral(String varName) throws JenaDriverException {
        final RDFNode value = current.get(varName);
        assert value != null;
        if (!value.isLiteral()) {
            throw new JenaDriverException("Expected value " + value + " to be a literal.");
        }
        return current.get(varName).asLiteral();
    }

    @Override
    public byte getByte(int variableIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public byte getByte(String variableName) throws OntoDriverException {
        return 0;
    }

    @Override
    public double getDouble(int variableIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public double getDouble(String variableName) throws OntoDriverException {
        return 0;
    }

    @Override
    public float getFloat(int variableIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public float getFloat(String variableName) throws OntoDriverException {
        return 0;
    }

    @Override
    public int getInt(int variableIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public int getInt(String variableName) throws OntoDriverException {
        return 0;
    }

    @Override
    public long getLong(int variableIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public long getLong(String variableName) throws OntoDriverException {
        return 0;
    }

    @Override
    public Object getObject(int variableIndex) throws OntoDriverException {
        return null;
    }

    @Override
    public Object getObject(String variableName) throws OntoDriverException {
        return null;
    }

    @Override
    public <T> T getObject(int variableIndex, Class<T> cls) throws OntoDriverException {
        return null;
    }

    @Override
    public <T> T getObject(String variableName, Class<T> cls) throws OntoDriverException {
        return null;
    }

    @Override
    public short getShort(int variableIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public short getShort(String variableName) throws OntoDriverException {
        return 0;
    }

    @Override
    public String getString(int variableIndex) {
        ensureState();
        return getString(getVariableAt(variableIndex));
    }

    private String getVariableAt(int index) {
        if (index < 0 || index >= jenaResult.getResultVars().size()) {
            throw new IllegalArgumentException("Variable index " + index + " is out of bounds.");
        }
        return jenaResult.getResultVars().get(index);
    }

    @Override
    public String getString(String variableName) {
        ensureState();
        ensureVariableExists(variableName);
        final RDFNode value = current.get(variableName);
        if (value.isResource()) {
            return value.asResource().getURI();
        } else {
            return value.asLiteral().getString();
        }
    }

    private void ensureVariableExists(String name) {
        if (!current.contains(name)) {
            throw new IllegalArgumentException("Variable \'" + name + "\' not found in the result set.");
        }
    }

    @Override
    public boolean isFirst() {
        ensureOpen();
        return rowIndex == 0;
    }

    @Override
    public boolean hasNext() {
        ensureOpen();
        return jenaResult.hasNext();
    }

    @Override
    public void last() {
        ensureOpen();
        while (hasNext()) {
            next();
        }
    }

    @Override
    public void next() {
        ensureOpen();
        if (!hasNext()) {
            throw new NoSuchElementException("No more rows found in this result set.");
        }
        this.current = jenaResult.next();
        this.rowIndex++;
    }

    @Override
    public void previous() {
        throw new UnsupportedOperationException("Moving back is not supported by this result set.");
    }

    @Override
    public void registerObserver(Observer observer) {

    }

    @Override
    public void relative(int rows) {
        ensureOpen();
        setRowIndex(rowIndex + rows);
    }

    @Override
    public int getRowIndex() {
        return rowIndex;
    }

    @Override
    public void setRowIndex(int newIndex) {
        ensureOpen();
        if (newIndex < rowIndex) {
            throw new UnsupportedOperationException("Moving back is not supported by this result set.");
        }
        while (rowIndex < newIndex) {
            next();
        }
    }

    @Override
    public void close() throws JenaDriverException {
        try {
            execution.close();
            this.open = false;
        } catch (RuntimeException e) {
            throw new JenaDriverException("Unable to close result set.", e);
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
