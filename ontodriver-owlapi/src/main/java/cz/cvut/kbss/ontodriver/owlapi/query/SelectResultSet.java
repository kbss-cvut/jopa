package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.BindingValueMismatchException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.owl2query.model.GroundTerm;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.ResultBinding;
import cz.cvut.kbss.owl2query.model.Variable;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObject;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.*;

class SelectResultSet extends AbstractResultSet {

    private final QueryResult<OWLObject> queryResult;
    private Iterator<ResultBinding<OWLObject>> iterator;

    private final Map<String, Variable<OWLObject>> namesToVariables;
    private final Map<Integer, Variable<OWLObject>> indexesToVariables;

    private int currentIndex;
    private ResultBinding<OWLObject> currentRow;

    public SelectResultSet(QueryResult<OWLObject> queryResult, Statement statement) {
        super(statement);
        this.queryResult = queryResult;
        this.iterator = queryResult.iterator();
        this.currentIndex = -1;
        final int bindingSize = queryResult.getResultVars().size();
        this.namesToVariables = new HashMap<>(bindingSize);
        this.indexesToVariables = new HashMap<>(bindingSize);
        resolveVariableNamesAndIndexes();
    }

    private void resolveVariableNamesAndIndexes() {
        Integer i = 0;
        for (Variable<OWLObject> v : queryResult.getResultVars()) {
            namesToVariables.put(v.getName(), v);
            indexesToVariables.put(i, v);
            i++;
        }
    }

    @Override
    public int findColumn(String columnLabel) {
        ensureOpen();
        final Variable<OWLObject> v = namesToVariables.get(columnLabel);
        if (v == null) {
            return -1;
        }
        for (Map.Entry<Integer, Variable<OWLObject>> e : indexesToVariables.entrySet()) {
            if (e.getValue().equals(v)) {
                return e.getKey();
            }
        }
        return -1;
    }

    @Override
    public int getColumnCount() {
        ensureOpen();
        return namesToVariables.size();
    }

    @Override
    public void first() throws OntoDriverException {
        ensureOpen();
        this.currentIndex = -1;
        this.iterator = queryResult.iterator();
        next();
    }

    @Override
    public boolean getBoolean(int columnIndex) throws OntoDriverException {
        return getPrimitiveValue(Boolean.class, columnIndex);
    }

    private <T> T getPrimitiveValue(Class<T> cls, int columnIndex) throws OntoDriverException {
        final Object val = OwlapiUtils.owlLiteralToValue(getLiteral(columnIndex));
        if (!cls.isAssignableFrom(val.getClass())) {
            throw new BindingValueMismatchException("Value " + val + " cannot be returned as " + cls.getSimpleName());
        }
        return cls.cast(val);
    }

    private OWLLiteral getLiteral(int columnIndex) throws OntoDriverException {
        final OWLObject currentValue = getCurrentValue(columnIndex);
        if (!(currentValue instanceof OWLLiteral)) {
            throw new BindingValueMismatchException("Value " + currentValue + " is not an OWLLiteral.");
        }
        return (OWLLiteral) currentValue;
    }

    private OWLObject getCurrentValue(int columnIndex) throws OntoDriverException {
        ensureOpen();
        if (currentRow == null) {
            throw new IllegalStateException("Current row is null.");
        }
        if (!indexesToVariables.containsKey(columnIndex)) {
            throw new OntoDriverException("No result binding found for index " + columnIndex);
        }
        final Variable<OWLObject> v = indexesToVariables.get(columnIndex);
        final GroundTerm<OWLObject> gt = currentRow.get(v);
        return gt.getWrappedObject();
    }

    @Override
    public boolean getBoolean(String columnLabel) throws OntoDriverException {
        return getPrimitiveValue(Boolean.class, columnLabel);
    }

    private <T> T getPrimitiveValue(Class<T> cls, String columnLabel) throws OwlapiDriverException {
        final Object val = OwlapiUtils.owlLiteralToValue(getLiteral(columnLabel));
        if (!cls.isAssignableFrom(val.getClass())) {
            throw new BindingValueMismatchException("Value " + val + " cannot be returned as " + cls.getSimpleName());
        }
        return cls.cast(val);
    }

    private OWLLiteral getLiteral(String columnLabel) throws OwlapiDriverException {
        final OWLObject currentValue = getCurrentValue(columnLabel);
        if (!(currentValue instanceof OWLLiteral)) {
            throw new BindingValueMismatchException("Value " + currentValue + " is not an OWLLiteral.");
        }
        return (OWLLiteral) currentValue;
    }

    private OWLObject getCurrentValue(String columnLabel) throws OwlapiDriverException {
        ensureOpen();
        if (currentRow == null) {
            throw new IllegalStateException("Current row is null.");
        }
        if (!namesToVariables.containsKey(columnLabel)) {
            throw new OwlapiDriverException("No result binding found for label " + columnLabel);
        }
        final Variable<OWLObject> v = namesToVariables.get(columnLabel);
        final GroundTerm<OWLObject> gt = currentRow.get(v);
        return gt.getWrappedObject();
    }

    @Override
    public byte getByte(int columnIndex) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnIndex);
        return num.byteValue();
    }

    @Override
    public byte getByte(String columnLabel) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnLabel);
        return num.byteValue();
    }

    @Override
    public double getDouble(int columnIndex) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnIndex);
        return num.doubleValue();
    }

    @Override
    public double getDouble(String columnLabel) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnLabel);
        return num.doubleValue();
    }

    @Override
    public float getFloat(int columnIndex) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnIndex);
        return num.floatValue();
    }

    @Override
    public float getFloat(String columnLabel) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnLabel);
        return num.floatValue();
    }

    @Override
    public int getInt(int columnIndex) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnIndex);
        return num.intValue();
    }

    @Override
    public int getInt(String columnLabel) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnLabel);
        return num.intValue();
    }

    @Override
    public long getLong(int columnIndex) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnIndex);
        return num.longValue();
    }

    @Override
    public long getLong(String columnLabel) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnLabel);
        return num.longValue();
    }

    @Override
    public Object getObject(int columnIndex) throws OntoDriverException {
        return owlObjectToObject(getCurrentValue(columnIndex));
    }

    private Object owlObjectToObject(OWLObject owlValue) {
        if (owlValue instanceof OWLLiteral) {
            return OwlapiUtils.owlLiteralToValue((OWLLiteral) owlValue);
        }
        final Set<OWLEntity> sig = owlValue.getSignature();
        if (sig.isEmpty()) {
            return owlValue.toString();
        } else {
            return URI.create(sig.iterator().next().toStringID());
        }
    }

    @Override
    public Object getObject(String columnLabel) throws OntoDriverException {
        return owlObjectToObject(getCurrentValue(columnLabel));
    }

    @Override
    public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
        Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
        return owlObjectToType(getCurrentValue(columnIndex), cls);
    }

    private <T> T owlObjectToType(OWLObject owlValue, Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(owlValue.getClass())) {
            return cls.cast(owlValue);
        }
        if (owlValue instanceof OWLLiteral) {
            final Object ob = OwlapiUtils.owlLiteralToValue((OWLLiteral) owlValue);
            if (cls.isAssignableFrom(ob.getClass())) {
                return cls.cast(ob);
            }
        } else {
            final Set<OWLEntity> sig = owlValue.getSignature();
            if (!sig.isEmpty()) {
                final URI uri = URI.create(sig.iterator().next().toStringID());
                if (cls.isAssignableFrom(uri.getClass())) {
                    return cls.cast(uri);
                }
                return tryInstantiatingClassUsingConstructor(cls, uri);
            }
        }
        throw new OwlapiDriverException("Conversion to type " + cls + " is not supported.");
    }

    private <T> T tryInstantiatingClassUsingConstructor(Class<T> cls, URI uri) throws OwlapiDriverException {
        try {
            final Constructor<T> constructor = cls.getDeclaredConstructor(uri.getClass());
            if (!constructor.isAccessible()) {
                constructor.setAccessible(true);
            }
            return constructor.newInstance(uri);
        } catch (NoSuchMethodException e) {
            throw new OwlapiDriverException(
                    "No constructor taking parameter of type " + uri.getClass().getName() + " found in class " + cls,
                    e);
        } catch (InvocationTargetException | InstantiationException | IllegalAccessException e) {
            throw new OwlapiDriverException(
                    "Unable to create instance of class " + cls + " using constructor with argument " + uri, e);
        }
    }

    @Override
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
        Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
        return owlObjectToType(getCurrentValue(columnLabel), cls);
    }

    @Override
    public int getRowIndex() throws OntoDriverException {
        return currentIndex;
    }

    @Override
    public short getShort(int columnIndex) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnIndex);
        return num.shortValue();
    }

    @Override
    public short getShort(String columnLabel) throws OntoDriverException {
        final Number num = getPrimitiveValue(Number.class, columnLabel);
        return num.shortValue();
    }

    @Override
    public String getString(int columnIndex) throws OntoDriverException {
        return owlValueToString(getCurrentValue(columnIndex));
    }

    private String owlValueToString(OWLObject owlValue) {
        if (owlValue instanceof OWLLiteral) {
            return OwlapiUtils.owlLiteralToValue((OWLLiteral) owlValue).toString();
        }
        final Set<OWLEntity> sig = owlValue.getSignature();
        if (sig.isEmpty()) {
            return owlValue.toString();
        } else {
            return sig.iterator().next().toStringID();
        }
    }

    @Override
    public String getString(String columnLabel) throws OntoDriverException {
        return owlValueToString(getCurrentValue(columnLabel));
    }

    @Override
    public boolean isFirst() throws OntoDriverException {
        ensureOpen();
        return currentIndex == 0;
    }

    @Override
    public boolean hasNext() throws OntoDriverException {
        ensureOpen();
        return iterator.hasNext();
    }

    @Override
    public void last() throws OntoDriverException {
        ensureOpen();
        while (hasNext()) {
            next();
        }
    }

    @Override
    public void next() throws OntoDriverException {
        ensureOpen();
        if (!hasNext()) {
            throw new NoSuchElementException("The result set has no more rows.");
        }
        this.currentRow = iterator.next();
        currentIndex++;
    }

    @Override
    public void previous() throws OntoDriverException {
        ensureOpen();
        relative(-1);
    }

    @Override
    public void registerObserver(Observer observer) throws OntoDriverException {
        // Not implemented yet
    }

    @Override
    public void relative(int rows) throws OntoDriverException {
        ensureOpen();
        setRowIndex(currentIndex + rows);
    }

    @Override
    public void setRowIndex(int rowIndex) throws OntoDriverException {
        ensureOpen();
        if (rowIndex == currentIndex) {
            return;
        }
        if (rowIndex < 0) {
            throw new IllegalArgumentException("Cannot set row index to a number less than 0.");
        }
        if (rowIndex < currentIndex) {
            first();
        }
        while (rowIndex > currentIndex && hasNext()) {
            next();
        }
    }
}
