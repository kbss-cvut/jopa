package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.apache.http.HttpConnection;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.HttpRequestRetryHandler;
import org.apache.http.client.ServiceUnavailableRetryStrategy;
import org.apache.http.client.config.CookieSpecs;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.protocol.HttpContext;
import org.eclipse.rdf4j.http.client.SharedHttpClientSessionManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.HttpURLConnection;

class HttpClientFactory {

    /**
     * Creates a customized {@link HttpClient} instance.
     * <p>
     * The client's configuration is basically the same as the default one used by RDF4J, but it sets a timeout on
     * connection requests from the connection pool, so that an application with exhausted connection pool fails
     * gracefully instead of potentially going into a deadlock.
     *
     * @param configuration Driver configuration
     *
     * @return HttpClient instance with connection pool request timeout
     */
    static HttpClient createHttpClient(DriverConfiguration configuration) {
        final RequestConfig customRequestConfig = RequestConfig.custom()
                                                               .setCookieSpec(CookieSpecs.STANDARD)
                                                               .setConnectionRequestTimeout(1000).build();
        // TODO Configurable connection request timeout
        // TODO Set larger max connections and max connections per route
        return HttpClientBuilder.create()
                                .evictExpiredConnections()
                                .setRetryHandler(new RetryHandlerStale())
                                .setServiceUnavailableRetryStrategy(new ServiceUnavailableRetryHandler())
                                .useSystemProperties()
                                .setDefaultRequestConfig(customRequestConfig).build();
    }

    /**
     * Copied from {@link SharedHttpClientSessionManager}
     */
    private static class RetryHandlerStale implements HttpRequestRetryHandler {
        private final Logger logger = LoggerFactory.getLogger(RetryHandlerStale.class);

        @Override
        public boolean retryRequest(IOException ioe, int count, HttpContext context) {
            // only try this once
            if (count > 1) {
                return false;
            }
            HttpClientContext clientContext = HttpClientContext.adapt(context);
            HttpConnection conn = clientContext.getConnection();
            if (conn != null) {
                synchronized (this) {
                    if (conn.isStale()) {
                        try {
                            logger.warn("Closing stale connection");
                            conn.close();
                            return true;
                        } catch (IOException e) {
                            logger.error("Error closing stale connection", e);
                        }
                    }
                }
            }
            return false;
        }
    }

    /**
     * Copied from {@link SharedHttpClientSessionManager}
     */
    private static class ServiceUnavailableRetryHandler implements ServiceUnavailableRetryStrategy {
        private final Logger logger = LoggerFactory.getLogger(ServiceUnavailableRetryHandler.class);

        @Override
        public boolean retryRequest(HttpResponse response, int executionCount, HttpContext context) {
            // only retry on `408`
            if (response.getStatusLine().getStatusCode() != HttpURLConnection.HTTP_CLIENT_TIMEOUT) {
                return false;
            }

            // when `keepAlive` is disabled every connection is fresh (with the default `useSystemProperties` http
            // client configuration we use), a 408 in that case is an unexpected issue we don't handle here
            String keepAlive = System.getProperty("http.keepAlive", "true");
            if (!"true".equalsIgnoreCase(keepAlive)) {
                return false;
            }

            // worst case, the connection pool is filled to the max and all of them idled out on the server already
            // we then need to clean up the pool and finally retry with a fresh connection. Hence, we need at most
            // pooledConnections+1 retries.
            // the pool size setting used here is taken from `HttpClientBuilder` when `useSystemProperties()` is used
            int pooledConnections = Integer.parseInt(System.getProperty("http.maxConnections", "5"));
            if (executionCount > (pooledConnections + 1)) {
                return false;
            }

            HttpClientContext clientContext = HttpClientContext.adapt(context);
            HttpConnection conn = clientContext.getConnection();

            synchronized (this) {
                try {
                    logger.info("Cleaning up closed connection");
                    conn.close();
                    return true;
                } catch (IOException e) {
                    logger.error("Error cleaning up closed connection", e);
                }
            }
            return false;
        }

        @Override
        public long getRetryInterval() {
            return 1000;
        }
    }
}
