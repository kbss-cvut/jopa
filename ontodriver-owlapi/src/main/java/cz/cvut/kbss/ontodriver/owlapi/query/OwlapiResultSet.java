package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.ResultSet;

import java.util.Observer;

public class OwlapiResultSet implements ResultSet {
    @Override
    public int findColumn(String columnLabel) {
        return 0;
    }

    @Override
    public int getColumnCount() {
        return 0;
    }

    @Override
    public void first() throws OntoDriverException {

    }

    @Override
    public boolean getBoolean(int columnIndex) throws OntoDriverException {
        return false;
    }

    @Override
    public boolean getBoolean(String columnLabel) throws OntoDriverException {
        return false;
    }

    @Override
    public byte getByte(int columnIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public byte getByte(String columnLabel) throws OntoDriverException {
        return 0;
    }

    @Override
    public double getDouble(int columnIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public double getDouble(String columnLabel) throws OntoDriverException {
        return 0;
    }

    @Override
    public float getFloat(int columnIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public float getFloat(String columnLabel) throws OntoDriverException {
        return 0;
    }

    @Override
    public int getInt(int columnIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public int getInt(String columnLabel) throws OntoDriverException {
        return 0;
    }

    @Override
    public long getLong(int columnIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public long getLong(String columnLabel) throws OntoDriverException {
        return 0;
    }

    @Override
    public Object getObject(int columnIndex) throws OntoDriverException {
        return null;
    }

    @Override
    public Object getObject(String columnLabel) throws OntoDriverException {
        return null;
    }

    @Override
    public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException {
        return null;
    }

    @Override
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException {
        return null;
    }

    @Override
    public int getRowIndex() throws OntoDriverException {
        return 0;
    }

    @Override
    public short getShort(int columnIndex) throws OntoDriverException {
        return 0;
    }

    @Override
    public short getShort(String columnLabel) throws OntoDriverException {
        return 0;
    }

    @Override
    public Statement getStatement() throws OntoDriverException {
        return null;
    }

    @Override
    public String getString(int columnIndex) throws OntoDriverException {
        return null;
    }

    @Override
    public String getString(String columnLabel) throws OntoDriverException {
        return null;
    }

    @Override
    public boolean isFirst() throws OntoDriverException {
        return false;
    }

    @Override
    public boolean hasNext() throws OntoDriverException {
        return false;
    }

    @Override
    public void last() throws OntoDriverException {

    }

    @Override
    public void next() throws OntoDriverException {

    }

    @Override
    public void previous() throws OntoDriverException {

    }

    @Override
    public void registerObserver(Observer observer) throws OntoDriverException {

    }

    @Override
    public void relative(int rows) throws OntoDriverException {

    }

    @Override
    public void setRowIndex(int rowIndex) throws OntoDriverException {

    }

    @Override
    public void close() throws OntoDriverException {

    }

    @Override
    public boolean isOpen() {
        return false;
    }
}
