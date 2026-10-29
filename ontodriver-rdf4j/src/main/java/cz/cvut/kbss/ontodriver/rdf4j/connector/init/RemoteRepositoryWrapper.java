/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.http.client.HttpClientSessionManager;
import org.eclipse.rdf4j.http.client.SharedHttpClientSessionManager;
import org.eclipse.rdf4j.http.client.spi.RDF4JHttpClient;
import org.eclipse.rdf4j.http.client.spi.RDF4JHttpClientConfig;
import org.eclipse.rdf4j.http.client.spi.RDF4JHttpClients;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import java.io.File;

/**
 * Wrapper for RDF4J {@link HTTPRepository} allowing to set custom {@link RDF4JHttpClient} for it to use.
 */
public class RemoteRepositoryWrapper implements Repository, Wrapper {

    private final HTTPRepository delegate;
    private RDF4JHttpClient httpClient;

    public RemoteRepositoryWrapper(HTTPRepository delegate, DriverConfiguration configuration) {
        this.delegate = delegate;
        this.httpClient = initHttpClient(configuration);
        final HttpClientSessionManager sessionManager = delegate.getHttpClientSessionManager();
        if (sessionManager instanceof SharedHttpClientSessionManager) {
            sessionManager.setHttpClient(httpClient);
        }
    }

    private RDF4JHttpClient initHttpClient(DriverConfiguration configuration) {
        final int maxConnections = getMaxConnections(configuration);
        final RDF4JHttpClientConfig rdf4jConfig = RDF4JHttpClientConfig.newBuilder()
                .connectionRequestTimeoutMs(getConnectionRequestTimeout(configuration))
                .maxConnectionsPerRoute(maxConnections)
                .maxConnectionsTotal(maxConnections)
                .build();
        return RDF4JHttpClients.newDefaultClient(rdf4jConfig);
    }

    private static int getConnectionRequestTimeout(DriverConfiguration config) {
        return config.getProperty(Rdf4jConfigParam.CONNECTION_REQUEST_TIMEOUT, Constants.DEFAULT_CONNECTION_REQUEST_TIMEOUT);
    }

    private static int getMaxConnections(DriverConfiguration config) {
        final int defaultMaxConnections = Math.max(Constants.DEFAULT_MAX_CONNECTIONS, Runtime.getRuntime()
                                                                                             .availableProcessors() * 2);
        return config.getProperty(Rdf4jConfigParam.MAX_CONNECTION_POOL_SIZE, defaultMaxConnections);
    }

    @Override
    public void setDataDir(File file) {
        delegate.setDataDir(file);
    }

    @Override
    public File getDataDir() {
        return delegate.getDataDir();
    }

    @Override
    public void init() throws RepositoryException {
        delegate.init();
    }

    @Override
    public boolean isInitialized() {
        return delegate.isInitialized();
    }

    @Override
    public void shutDown() throws RepositoryException {
        delegate.shutDown();
        if (httpClient != null) {
            httpClient.close();
            this.httpClient = null;
        }
    }

    @Override
    public boolean isWritable() throws RepositoryException {
        return delegate.isWritable();
    }

    @Override
    public RepositoryConnection getConnection() throws RepositoryException {
        return delegate.getConnection();
    }

    @Override
    public ValueFactory getValueFactory() {
        return delegate.getValueFactory();
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(delegate.getClass())) {
            return cls.cast(delegate);
        }
        throw new Rdf4jDriverException("No instance of class " + cls + " found.");
    }
}
