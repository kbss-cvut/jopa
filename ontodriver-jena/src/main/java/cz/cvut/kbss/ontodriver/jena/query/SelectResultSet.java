/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.jena.query;


import cz.cvut.kbss.jopa.datatype.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.exception.VariableNotBoundException;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.RDFNode;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public class SelectResultSet extends AbstractResultSet {

    private final QueryExecution execution;
    private final ResultSet jenaResult;
    private QuerySolution current;

    public SelectResultSet(QueryExecution execution, ResultSet jenaResult) {
        this.execution = execution;
        this.jenaResult = jenaResult;
    }

    @Override
    public int findColumn(String variableName) {
        ensureOpen();
        return jenaResult.getResultVars().indexOf(variableName);
    }

    @Override
    public int getColumnCount() {
        ensureOpen();
        return jenaResult.getResultVars().size();
    }

    @Override
    public List<String> getColumnNames() {
        return Collections.unmodifiableList(jenaResult.getResultVars());
    }

    @Override
    public boolean isBound(int variableIndex) {
        ensureState();
        return variableIndex >= 0 && variableIndex < jenaResult.getResultVars().size() && current
                .get(getVariableAt(variableIndex)) != null;
    }

    @Override
    public boolean isBound(String variableName) {
        ensureState();
        return jenaResult.getResultVars().contains(variableName) && current.get(variableName) != null;
    }

    @Override
    public boolean getBoolean(int variableIndex) throws JenaDriverException {
        ensureState();
        return getBoolean(getVariableAt(variableIndex));
    }

    @Override
    public boolean getBoolean(String variableName) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        return getLiteral(variableName).getBoolean();
    }

    private Literal getLiteral(String varName) throws JenaDriverException {
        Objects.requireNonNull(varName);
        assert current != null;
        final RDFNode value = getCurrent(varName);
        if (!value.isLiteral()) {
            throw new JenaDriverException("Expected value " + value + " to be a literal.");
        }
        return current.get(varName).asLiteral();
    }

    private RDFNode getCurrent(String varName) {
        final RDFNode value = current.get(varName);
        if (value == null) {
            throw new VariableNotBoundException("Variable " + varName + " is not bound in the current result row.");
        }
        return value;
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
        assert current != null;
        return toObject(getCurrent(getVariableAt(variableIndex)));
    }

    private static Object toObject(RDFNode value) {
        assert value != null;
        if (value.isLiteral()) {
            return JenaUtils.literalToValue(value.asLiteral());
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
        assert current != null;
        return toObject(getCurrent(Objects.requireNonNull(variableName)));
    }

    @Override
    public <T> T getObject(int variableIndex, Class<T> cls) throws JenaDriverException {
        ensureState();
        assert current != null;
        return toObject(getCurrent(getVariableAt(variableIndex)), cls);
    }

    private static <T> T toObject(RDFNode value, Class<T> cls) {
        Objects.requireNonNull(cls);
        if (cls.isAssignableFrom(value.getClass())) {
            return cls.cast(value);
        }
        Object objectValue;
        if (value.isLiteral()) {
            objectValue = JenaUtils.literalToValue(value.asLiteral());
        } else {
            assert value.isResource();
            if (value.isURIResource()) {
                objectValue = URI.create(value.asResource().getURI());
            } else {
                objectValue = value.asResource().getId().getLabelString();
            }
        }
        return DatatypeTransformer.transform(objectValue, cls);
    }

    @Override
    public <T> T getObject(String variableName, Class<T> cls) throws JenaDriverException {
        ensureState();
        ensureVariableExists(variableName);
        assert current != null;
        return toObject(getCurrent(variableName), cls);
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
        assert current != null;
        final RDFNode value = getCurrent(variableName);
        if (value.isResource()) {
            return value.isURIResource() ? value.asResource().getURI() : value.asResource().getId().getLabelString();
        } else {
            return value.asLiteral().getString();
        }
    }

    private void ensureVariableExists(String name) {
        assert current != null;
        if (!current.contains(name)) {
            throw new IllegalArgumentException("Variable '" + name + "' not found in the result set.");
        }
    }

    @Override
    public boolean hasNext() {
        ensureOpen();
        return jenaResult.hasNext();
    }

    @Override
    public void next() {
        super.next();
        this.current = jenaResult.next();
    }

    @Override
    public void close() throws JenaDriverException {
        try {
            execution.close();
            super.close();
        } catch (RuntimeException e) {
            throw new JenaDriverException("Unable to close result set.", e);
        }
    }
}
