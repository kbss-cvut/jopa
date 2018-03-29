package cz.cvut.kbss.ontodriver.jena.query;


import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.RDFNode;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.NoSuchElementException;
import java.util.Objects;
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
        Objects.requireNonNull(varName);
        final RDFNode value = current.get(varName);
        assert value != null;
        if (!value.isLiteral()) {
            throw new JenaDriverException("Expected value " + value + " to be a literal.");
        }
        return current.get(varName).asLiteral();
    }

    @Override
    public byte getByte(int variableIndex) throws JenaDriverException {
        ensureState();
        return getLiteral(getVariableAt(variableIndex)).getByte();
    }

    @Override
    public byte getByte(String variableName) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return getLiteral(variableName).getByte();
    }

    @Override
    public double getDouble(int variableIndex) throws JenaDriverException {
        ensureState();
        return getLiteral(getVariableAt(variableIndex)).getDouble();
    }

    @Override
    public double getDouble(String variableName) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return getLiteral(variableName).getDouble();
    }

    @Override
    public float getFloat(int variableIndex) throws JenaDriverException {
        ensureState();
        return getLiteral(getVariableAt(variableIndex)).getFloat();
    }

    @Override
    public float getFloat(String variableName) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return getLiteral(variableName).getFloat();
    }

    @Override
    public int getInt(int variableIndex) throws JenaDriverException {
        ensureState();
        return getLiteral(getVariableAt(variableIndex)).getInt();
    }

    @Override
    public int getInt(String variableName) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return getLiteral(variableName).getInt();
    }

    @Override
    public long getLong(int variableIndex) throws JenaDriverException {
        ensureState();
        return getLiteral(getVariableAt(variableIndex)).getLong();
    }

    @Override
    public long getLong(String variableName) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return getLiteral(variableName).getLong();
    }

    @Override
    public Object getObject(int variableIndex) {
        ensureState();
        return toObject(current.get(getVariableAt(variableIndex)));
    }

    private Object toObject(RDFNode value) {
        if (value.isLiteral()) {
            return value.asLiteral().getValue();
        } else {
            assert value.isResource();
            if (value.isURIResource()) {
                return URI.create(value.asResource().getURI());
            } else {
                return value.asResource().getId().getLabelString();
            }
        }
    }

    @Override
    public Object getObject(String variableName) {
        ensureState();
        ensureVariableExists(variableName);
        return toObject(current.get(Objects.requireNonNull(variableName)));
    }

    @Override
    public <T> T getObject(int variableIndex, Class<T> cls) throws JenaDriverException {
        ensureState();
        return toObject(current.get(getVariableAt(variableIndex)), cls);
    }

    private <T> T toObject(RDFNode value, Class<T> cls) throws JenaDriverException {
        Objects.requireNonNull(cls);
        if (cls.isAssignableFrom(value.getClass())) {
            return cls.cast(value);
        }
        Object objectValue;
        if (value.isLiteral()) {
            objectValue = value.asLiteral().getValue();
        } else {
            assert value.isResource();
            if (value.isURIResource()) {
                objectValue = URI.create(value.asResource().getURI());
            } else {
                objectValue = value.asResource().getId().getLabelString();
            }
        }
        if (objectValue != null && cls.isAssignableFrom(objectValue.getClass())) {
            return cls.cast(objectValue);
        } else {
            return buildUsingConstructor(cls, value, objectValue);
        }
    }

    private <T> T buildUsingConstructor(Class<T> cls, RDFNode jenaValue, Object javaValue) throws JenaDriverException {
        final Constructor<?>[] constructors = cls.getDeclaredConstructors();
        try {
            for (Constructor<?> c : constructors) {
                if (c.getParameterCount() != 1) {
                    continue;
                }
                if (c.getParameterTypes()[0].isAssignableFrom(jenaValue.getClass())) {
                    return cls.cast(c.newInstance(jenaValue));
                }
                if (c.getParameterTypes()[0].isAssignableFrom(javaValue.getClass())) {
                    return cls.cast(c.newInstance(javaValue));
                }
            }
        } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
            throw new JenaDriverException("Unable to instantiate class " + cls + " with value " + jenaValue, e);
        }
        throw new JenaDriverException("No suitable constructor for value " + jenaValue + " found in type " + cls);
    }

    @Override
    public <T> T getObject(String variableName, Class<T> cls) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return toObject(current.get(variableName), cls);
    }

    @Override
    public short getShort(int variableIndex) throws JenaDriverException {
        ensureState();
        return getLiteral(getVariableAt(variableIndex)).getShort();
    }

    @Override
    public short getShort(String variableName) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return getLiteral(variableName).getShort();
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
        throw new UnsupportedOperationException("Not supported by the current version.");
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
