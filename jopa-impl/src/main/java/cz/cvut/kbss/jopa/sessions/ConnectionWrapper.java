package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapper;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapperImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Statement;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

public class ConnectionWrapper {

    private final Connection connection;
    private ObjectOntologyMapper mapper;

    public ConnectionWrapper(Connection connection) {
        this.connection = connection;
    }

    void setUnitOfWork(UnitOfWorkImpl uow) {
        this.mapper = new ObjectOntologyMapperImpl(uow, connection);
    }

    <T> boolean contains(Object primaryKey, Class<T> cls, Descriptor descriptor) {
        final URI pkUri = getPrimaryKeyAsUri(primaryKey);
        return mapper.containsEntity(cls, pkUri, descriptor);
    }

    <T> T find(LoadingParameters<T> loadingParameters) {
        return mapper.loadEntity(loadingParameters);
    }

    <T> void merge(T entity, Field field, Descriptor descriptor) {
        mapper.updateFieldValue(entity, field, descriptor);
    }

    <T> void persist(Object primaryKey, T entity, Descriptor descriptor) {
        final URI pkUri = getPrimaryKeyAsUri(primaryKey);
        mapper.persistEntity(pkUri, entity, descriptor);
    }

    <T> void remove(Object primaryKey, Class<T> cls, Descriptor descriptor) {
        final URI pkUri = getPrimaryKeyAsUri(primaryKey);
        mapper.removeEntity(pkUri, cls, descriptor);
    }

    <T> void loadFieldValue(T entity, Field field, Descriptor descriptor) {
        mapper.loadFieldValue(entity, field, descriptor);
    }

    void commit() {
        try {
            mapper.checkForUnpersistedChanges();
            connection.commit();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    void rollback() {
        try {
            connection.rollback();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    void close() {
        try {
            connection.close();
        } catch (Exception e) {
            throw new OWLPersistenceException(e);
        }
    }

    boolean isConsistent(URI context) {
        try {
            return connection.isConsistent(context);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    List<URI> getContexts() {
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

    private URI getPrimaryKeyAsUri(Object primaryKey) {
        return primaryKey == null ? null : EntityPropertiesUtils.getValueAsURI(primaryKey);
    }
}
