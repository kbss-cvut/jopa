package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.logging.Level;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

class LegacyConnectionWrapper extends ConnectionWrapper {

    private Connection connection;

    LegacyConnectionWrapper(Connection connection) {
        this.connection = connection;
    }

    @Override
    <T> boolean contains(Object primaryKey, Class<T> cls, Descriptor descriptor) {
        assert primaryKey != null;
        try {
            return connection.contains(primaryKey, descriptor.getContext());
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    <T> T find(LoadingParameters<T> loadingParameters) {
        assert loadingParameters != null;
        try {
            final T result = connection.find(loadingParameters.getEntityType(), loadingParameters.getIdentifier(), loadingParameters.getDescriptor());
            if (result != null) {
                // Put into cache here, when we are sure that the entity is in
                // the ontology
                uow.putObjectIntoCache(loadingParameters.getIdentifier(), result, loadingParameters.getDescriptor().getContext());
            }
            return result;
        } catch (MetamodelNotSetException | OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    <T> void merge(T entity, Field field, Descriptor repository) {
        assert entity != null;
        assert repository != null;
        try {
            connection.merge(entity, field, repository);
        } catch (MetamodelNotSetException | OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    <T> void persist(Object primaryKey, T entity, Descriptor descriptor) {
        assert entity != null;
        assert descriptor != null;
        try {
            connection.persist(primaryKey, entity, descriptor);
        } catch (MetamodelNotSetException | OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    <T> void remove(Object primaryKey, Class<T> cls, Descriptor descriptor) {
        assert primaryKey != null;
        assert descriptor != null;
        try {
            connection.remove(primaryKey, descriptor);
        } catch (MetamodelNotSetException | OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    <T> void loadFieldValue(T entity, Field field, Descriptor descriptor) {
        try {
            connection.loadFieldValue(entity, field, descriptor);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    void commit() {
        try {
            connection.commit();
        } catch (Exception e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    void rollback() {
        try {
            connection.rollback();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    void close() {
        try {
            connection.close();
        } catch (OntoDriverException e) {
            LOG.log(Level.SEVERE, "Exception caugth when closing connection.", e);
        }
    }

    @Override
    boolean isConsistent(URI context) {
        try {
            return connection.isConsistent(context);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    List<URI> getContexts() {
        try {
            return connection.getContexts();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    public Statement createStatement() {
        try {
            return connection.createStatement();
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }
}
