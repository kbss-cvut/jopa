/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapper;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapperImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.Wrapper;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.List;
import java.util.Set;

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

    public <T> T find(LoadingParameters<T> loadingParameters) {
        return mapper.loadEntity(loadingParameters);
    }

    public <T> T getReference(LoadingParameters<T> loadingParameters) {
        return mapper.loadReference(loadingParameters);
    }

    public <T> void merge(T entity, FieldSpecification<? super T, ?> fieldSpec, Descriptor descriptor) {
        mapper.updateFieldValue(entity, fieldSpec, descriptor);
    }

    public <T> void persist(Object identifier, T entity, Descriptor descriptor) {
        final URI idUri = getIdentifierAsUri(identifier);
        mapper.persistEntity(idUri, entity, descriptor);
    }

    public <T> void remove(Object identifier, Class<T> cls, Descriptor descriptor) {
        final URI idUri = getIdentifierAsUri(identifier);
        mapper.removeEntity(idUri, cls, descriptor);
    }

    public <T> void loadFieldValue(T entity, FieldSpecification<? super T, ?> fieldSpec, Descriptor descriptor) {
        mapper.loadFieldValue(entity, fieldSpec, descriptor);
    }

    public <T> Set<Axiom<?>> getAttributeAxioms(T entity, FieldSpecification<? super T, ?> fieldSpec,
                                                Descriptor entityDescriptor) {
        return mapper.getAttributeAxioms(entity, fieldSpec, entityDescriptor);
    }

    public <T> boolean isInferred(T entity, FieldSpecification<? super T, ?> fieldSpec, Object value,
                                  Descriptor entityDescriptor) {
        return mapper.isInferred(entity, fieldSpec, value, entityDescriptor);
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

    public boolean isInferred(Axiom<?> axiom, Set<URI> contexts) {
        try {
            return connection.isInferred(axiom, contexts);
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
