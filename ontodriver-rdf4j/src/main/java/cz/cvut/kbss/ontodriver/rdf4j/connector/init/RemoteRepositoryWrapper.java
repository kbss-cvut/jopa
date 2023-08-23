package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.utils.HttpClientUtils;
import org.eclipse.rdf4j.http.client.HttpClientSessionManager;
import org.eclipse.rdf4j.http.client.SharedHttpClientSessionManager;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import java.io.File;

/**
 * Wrapper for RDF4J {@link HTTPRepository} allowing to set custom {@link HttpClient} for it to use.
 */
public class RemoteRepositoryWrapper implements Repository, Wrapper {

    private final HTTPRepository delegate;
    private HttpClient httpClient;

    public RemoteRepositoryWrapper(HTTPRepository delegate, DriverConfiguration configuration) {
        this.delegate = delegate;
        this.httpClient = HttpClientFactory.createHttpClient(configuration);
        final HttpClientSessionManager sessionManager = delegate.getHttpClientSessionManager();
        if (sessionManager instanceof SharedHttpClientSessionManager) {
            ((SharedHttpClientSessionManager) sessionManager).setHttpClient(httpClient);
        }
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
            HttpClientUtils.closeQuietly(httpClient);
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
