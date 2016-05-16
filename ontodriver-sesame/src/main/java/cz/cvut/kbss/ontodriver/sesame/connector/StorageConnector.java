/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.exceptions.RepositoryCreationException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.RepositoryNotFoundException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import info.aduna.iteration.Iterations;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.query.*;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.RepositoryResult;
import org.openrdf.repository.config.RepositoryConfig;
import org.openrdf.repository.config.RepositoryConfigException;
import org.openrdf.repository.manager.RepositoryManager;
import org.openrdf.repository.manager.RepositoryProvider;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.repository.sail.config.SailRepositoryConfig;
import org.openrdf.sail.config.SailImplConfig;
import org.openrdf.sail.inferencer.fc.ForwardChainingRDFSInferencer;
import org.openrdf.sail.inferencer.fc.config.ForwardChainingRDFSInferencerConfig;
import org.openrdf.sail.memory.MemoryStore;
import org.openrdf.sail.nativerdf.config.NativeStoreConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collection;
import java.util.List;

class StorageConnector extends AbstractConnector {

    private static final String[] KNOWN_REMOTE_SCHEMES = {"http", "https", "ftp"};
    private static final String LOCAL_NATIVE_REPO = "repositories/";
    private static final String FILE_SCHEME = "file";

    private static final Logger LOG = LoggerFactory.getLogger(StorageConnector.class);

    private final OntologyStorageProperties storageProperties;

    private Repository repository;
    private RepositoryManager manager;
    private RepositoryConnection connection;

    StorageConnector(OntologyStorageProperties storageProperties, Configuration configuration)
            throws SesameDriverException {
        assert storageProperties != null;
        assert configuration != null;

        this.storageProperties = storageProperties;
        initialize(configuration);
        this.open = true;
    }

    private void initialize(Configuration configuration) throws SesameDriverException {
        final URI serverUri = storageProperties.getPhysicalURI();
        LOG.debug("Initializing connector to repository at {}", serverUri);
        try {
            final boolean isRemote = isRemoteRepository(serverUri);
            if (isRemote) {
                this.repository = RepositoryProvider.getRepository(serverUri.toString());
            } else {
                this.repository = createLocalRepository(configuration);
            }
            if (repository == null) {
                if (isRemote) {
                    throw new RepositoryNotFoundException("Unable to reach repository at "
                            + serverUri);
                } else {
                    throw new RepositoryCreationException("Unable to create local repository at "
                            + serverUri);
                }
            }
            repository.initialize();
        } catch (RepositoryException | RepositoryConfigException e) {
            throw new SesameDriverException("Failed to acquire sesame repository connection.", e);
        }
    }

    private Repository createLocalRepository(Configuration configuration) {
        final URI localUri = storageProperties.getPhysicalURI();
        if (!isFileUri(localUri) && configuration.is(SesameConfigParam.USE_VOLATILE_STORAGE)) {
            return createInMemoryRepository(configuration);
        } else {
            return createNativeRepository(configuration, localUri);
        }
    }

    /**
     * Creates a local in-memory Sesame repository which is disposed when the VM shuts down.
     */
    private Repository createInMemoryRepository(Configuration configuration) {
        LOG.trace("Creating local in-memory repository.");
        final MemoryStore ms = new MemoryStore();
        if (configuration.is(SesameConfigParam.USE_INFERENCE)) {
            return new SailRepository(new ForwardChainingRDFSInferencer(ms));
        } else {
            return new SailRepository(ms);
        }
    }

    /**
     * Creates native repository. </p>
     * <p>
     * This kind of repository stores data in files and is persistent after the VM shuts down.
     */
    private Repository createNativeRepository(Configuration configuration, final URI localUri) {
        LOG.trace("Creating local native repository at " + localUri);
        final String[] tmp = localUri.toString().split(LOCAL_NATIVE_REPO);
        if (tmp.length != 2) {
            throw new RepositoryCreationException(
                    "Unsupported local Sesame repository path. Expected file://path/repositories/id but got "
                            + localUri);
        }
        String repoId = tmp[1];
        if (repoId.charAt(repoId.length() - 1) == '/') {
            repoId = repoId.substring(0, repoId.length() - 1);
        }
        try {
            this.manager = RepositoryProvider.getRepositoryManagerOfRepository(localUri.toASCIIString());
            final RepositoryConfig cfg = createLocalNativeRepositoryConfig(repoId, configuration);
            manager.addRepositoryConfig(cfg);
            return manager.getRepository(repoId);
        } catch (RepositoryConfigException | RepositoryException e) {
            throw new RepositoryCreationException("Unable to create local repository at "
                    + localUri, e);
        }
    }

    private RepositoryConfig createLocalNativeRepositoryConfig(String repoId, Configuration configuration) {
        SailImplConfig backend = new NativeStoreConfig();
        if (configuration.is(SesameConfigParam.USE_INFERENCE)) {
            backend = new ForwardChainingRDFSInferencerConfig(backend);
        }
        final SailRepositoryConfig repoType = new SailRepositoryConfig(backend);
        return new RepositoryConfig(repoId, repoType);
    }

    @Override
    public void close() throws SesameDriverException {
        if (!open) {
            return;
        }
        LOG.debug("Closing connector to repository {}.", storageProperties.getPhysicalURI());
        try {
            repository.shutDown();
            if (manager != null) {
                manager.shutDown();
            }
        } catch (RepositoryException e) {
            throw new SesameDriverException(
                    "Exception caught when closing Sesame repository connection.", e);
        } finally {
            this.open = false;
        }
    }

    private static boolean isFileUri(URI uri) {
        return (uri.getScheme() != null && uri.getScheme().equals(FILE_SCHEME));
    }

    private static boolean isRemoteRepository(URI uri) {
        final String scheme = uri.getScheme();
        for (String s : KNOWN_REMOTE_SCHEMES) {
            if (s.equals(scheme)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public TupleQueryResult executeSelectQuery(String query) throws SesameDriverException {
        RepositoryConnection connection;
        try {
            connection = acquireConnection();
            final TupleQuery tq = connection.prepareTupleQuery(QueryLanguage.SPARQL, query);
            return new QueryResult(tq.evaluate(), connection);
        } catch (MalformedQueryException | QueryEvaluationException | RepositoryException e) {
            throw new SesameDriverException(e);
        }
        // The connection is released by the result set once it is closed
    }

    private RepositoryConnection acquireConnection() throws SesameDriverException {
        try {
            return repository.getConnection();
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    private void releaseConnection(RepositoryConnection connection) throws SesameDriverException {
        try {
            if (connection != null) {
                connection.close();
            }
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public boolean executeBooleanQuery(String query) throws SesameDriverException {
        RepositoryConnection connection = null;
        try {
            connection = acquireConnection();
            return connection.prepareBooleanQuery(QueryLanguage.SPARQL, query).evaluate();
        } catch (MalformedQueryException | QueryEvaluationException | RepositoryException e) {
            throw new SesameDriverException(e);
        } finally {
            releaseConnection(connection);
        }
    }

    @Override
    public void executeUpdate(String query) throws SesameDriverException {
        RepositoryConnection connection = null;
        try {
            connection = acquireConnection();
            final Update u = connection.prepareUpdate(QueryLanguage.SPARQL, query);
            u.execute();
        } catch (MalformedQueryException | UpdateExecutionException | RepositoryException e) {
            throw new SesameDriverException(e);
        } finally {
            releaseConnection(connection);
        }
    }

    @Override
    public List<Resource> getContexts() throws SesameDriverException {
        RepositoryConnection connection = null;
        try {
            connection = acquireConnection();
            final RepositoryResult<Resource> res = connection.getContextIDs();
            return Iterations.asList(res);
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        } finally {
            releaseConnection(connection);
        }
    }

    @Override
    public ValueFactory getValueFactory() {
        return repository.getValueFactory();
    }

    @Override
    public void begin() throws SesameDriverException {
        super.begin();
        this.connection = acquireConnection();
        try {
            connection.begin();
        } catch (RepositoryException e) {
            transaction.rollback();
            throw new SesameDriverException(e);
        }
    }

    @Override
    public void commit() throws SesameDriverException {
        assert connection != null;

        transaction.commit();
        try {
            connection.commit();
            connection.close();
            this.connection = null;
            transaction.afterCommit();
        } catch (RepositoryException e) {
            transaction.rollback();
            throw new SesameDriverException(e);
        }
    }

    @Override
    public void rollback() throws SesameDriverException {
        assert connection != null;
        transaction.rollback();
        try {
            connection.rollback();
            connection.close();
            this.connection = null;
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        } finally {
            transaction.afterRollback();
        }
    }

    @Override
    public void addStatements(Collection<Statement> statements) throws SesameDriverException {
        verifyTransactionActive();
        assert connection != null;

        try {
            connection.add(statements);
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public void removeStatements(Collection<Statement> statements) throws SesameDriverException {
        verifyTransactionActive();
        assert connection != null;

        try {
            connection.remove(statements);
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, org.openrdf.model.URI property,
                                                Value value, boolean includeInferred, org.openrdf.model.URI... contexts)
            throws SesameDriverException {
        RepositoryConnection connection = null;
        try {
            connection = acquireConnection();
            final RepositoryResult<Statement> m = connection.getStatements(subject, property, null,
                    includeInferred, contexts);
            return Iterations.asList(m);
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        } finally {
            releaseConnection(connection);
        }
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        if (cls.isAssignableFrom(repository.getClass())) {
            return cls.cast(repository);
        }
        throw new SesameDriverException("No instance of class " + cls + " found.");
    }
}
