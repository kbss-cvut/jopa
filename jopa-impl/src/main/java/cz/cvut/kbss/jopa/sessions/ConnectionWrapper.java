/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapper;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapperImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.Wrapper;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

public class ConnectionWrapper implements Wrapper {

    private final Connection connection;
    private ObjectOntologyMapper mapper;

    public ConnectionWrapper(Connection connection) {
        this.connection = connection;
    }

    void setUnitOfWork(UnitOfWorkImpl uow) {
        this.mapper = new ObjectOntologyMapperImpl(uow, connection);
    }

    public <T> boolean contains(Object identifier, Class<T> cls, Descriptor descriptor) {
        final URI idUri = getIdentifierAsUri(identifier);
        return idUri != null && mapper.containsEntity(cls, idUri, descriptor);
    }

    private static URI getIdentifierAsUri(Object identifier) {
        return identifier == null ? null : EntityPropertiesUtils.getValueAsURI(identifier);
    }

    public <T> FindResult<? extends T> find(LoadingParameters<T> loadingParameters) {
        return mapper.loadEntity(loadingParameters);
    }

    public <T> void merge(T entity, Field field, Descriptor descriptor) {
        mapper.updateFieldValue(entity, field, descriptor);
    }

    public <T> void persist(Object identifier, T entity, Descriptor descriptor) {
        final URI idUri = getIdentifierAsUri(identifier);
        mapper.persistEntity(idUri, entity, descriptor);
    }

    public <T> void remove(Object identifier, Class<T> cls, Descriptor descriptor) {
        final URI idUri = getIdentifierAsUri(identifier);
        mapper.removeEntity(idUri, cls, descriptor);
    }

    public <T> void loadFieldValue(T entity, Field field, Descriptor descriptor) {
        mapper.loadFieldValue(entity, field, descriptor);
    }

    public void commit() {
        try {
            mapper.checkForUnpersistedChanges();
            connection.commit();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    public void rollback() {
        try {
            connection.rollback();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    public void close() {
        try {
            connection.close();
        } catch (Exception e) {
            throw new OWLPersistenceException(e);
        }
    }

    public boolean isConsistent(URI context) {
        try {
            return connection.isConsistent(context);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    public List<URI> getContexts() {
        try {
            return connection.getContexts();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    public Statement createStatement() {
        try {
            return connection.createStatement();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        try {
            return connection.unwrap(cls);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }
}
