package cz.cvut.kbss.ontodriver.sesame.query;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

/**
 * @author kidney
 */
public class AskResultSet extends AbstractResultSet {

    private final boolean result;

    public AskResultSet(boolean result, Statement statement) {
        super(statement);
        this.result = result;
    }

    @Override
    public int findColumn(String columnLabel) {
        return 0;
    }

    @Override
    public int getColumnCount() {
        return 1;
    }

    // We discard column index and column name, because in a boolean result, there is no such concept. Therefore,
    // the result is returned for any column index and column name.

    @Override
    public boolean getBoolean(int columnIndex) throws OntoDriverException {
        ensureState();
        return result;
    }

    private void ensureState() throws OntoDriverException {
        ensureOpen();
        if (!isFirst()) {
            throw new IllegalStateException("Must call next before getting the first value.");
        }
    }

    @Override
    public boolean getBoolean(String columnLabel) throws OntoDriverException {
        ensureState();
        return result;
    }

    @Override
    public byte getByte(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("byte");
    }

    private UnsupportedOperationException unsupported(String type) {
        return new UnsupportedOperationException("ASK query results cannot return " + type + "values.");
    }

    @Override
    public byte getByte(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("byte");
    }

    @Override
    public double getDouble(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("double");
    }

    @Override
    public double getDouble(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("double");
    }

    @Override
    public float getFloat(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("float");
    }

    @Override
    public float getFloat(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("float");
    }

    @Override
    public int getInt(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("int");
    }

    @Override
    public int getInt(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("int");
    }

    @Override
    public long getLong(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("long");
    }

    @Override
    public long getLong(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("long");
    }

    @Override
    public Object getObject(int columnIndex) throws OntoDriverException {
        ensureState();
        return result;
    }

    @Override
    public Object getObject(String columnLabel) throws OntoDriverException {
        ensureState();
        return result;
    }

    @Override
    public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
        ensureState();
        return toType(cls);
    }

    @Override
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
        ensureState();
        return toType(cls);
    }

    private <T> T toType(Class<T> type) throws OntoDriverException {
        if (type.isAssignableFrom(Boolean.class)) {
            return type.cast(result);
        }
        if (type.isAssignableFrom(String.class)) {
            return type.cast(getString(0));
        }
        throw new SesameDriverException("Unable to return booelan result as type " + type);
    }

    @Override
    public short getShort(int columnIndex) throws OntoDriverException {
        ensureState();
        throw unsupported("short");
    }

    @Override
    public short getShort(String columnLabel) throws OntoDriverException {
        ensureState();
        throw unsupported("short");
    }

    @Override
    public String getString(int columnIndex) throws OntoDriverException {
        ensureState();
        return Boolean.toString(result);
    }

    @Override
    public String getString(String columnLabel) throws OntoDriverException {
        ensureState();
        return Boolean.toString(result);
    }

    @Override
    public boolean hasNext() throws OntoDriverException {
        ensureOpen();
        return !isFirst();
    }
}
