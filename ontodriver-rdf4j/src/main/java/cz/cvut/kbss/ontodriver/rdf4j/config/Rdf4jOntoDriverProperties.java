/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.config;

public abstract class Rdf4jOntoDriverProperties {

    /**
     * Specifies whether a in-memory storage should be used for local RDF4J repositories.
     * <p>
     * When set to true, any local RDF4J repositories that are created by the driver are created as only MemoryStores
     * without any persistent backend. Repositories accessed over the Internet or already existing locally are not
     * affected by this setting.
     * <p>
     * {@code Boolean} value expected, default is false.
     */
    public static final String USE_VOLATILE_STORAGE = "cz.cvut.kbss.ontodriver.rdf4j.use-volatile-storage";

    /**
     * Specifies whether RDF4J inference (RDFS, forward chaining) should be used.
     * <p>
     * Note that this setting applies only to local storages (in memory or native), remote storages use their own
     * inference settings.
     * <p>
     * {@code Boolean} value expected, default is false.
     */
    public static final String USE_INFERENCE = "cz.cvut.kbss.ontodriver.rdf4j.use-inference";

    /**
     * Specifies how many requested assertions suffice to perform load all.
     * <p>
     * More specifically, if the number of assertions requested by an {@link cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor}
     * is low enough, the statements for them will be loaded by asking RDF4J for statements with subject and property
     * bound.
     * <p>
     * Otherwise, statements will be loaded using only subject bound and will be filtered according to the assertions.
     * This will in most cases have better performance than loading with bound property.
     */
    public static final String LOAD_ALL_THRESHOLD = "cz.cvut.kbss.ontodriver.rdf4j.load-all-threshold";

    /**
     * Path to repository configuration file.
     * <p>
     * The configuration file is a Turtle file corresponding to the repository config file schema defined by RDF4J. See
     * for example <a href="http://docs.rdf4j.org/server-workbench-console/#_repository_configuration_templates_advanced">
     * http://docs.rdf4j.org/server-workbench-console/#_repository_configuration_templates_advanced</a>
     * <p>
     * Sample configuration files are available in the RDF4J GitHub repo:
     * <a href="https://github.com/eclipse/rdf4j/tree/master/repository/api/src/main/resources/org/eclipse/rdf4j/repository/config">
     * https://github.com/eclipse/rdf4j/tree/master/repository/api/src/main/resources/org/eclipse/rdf4j/repository/config</a>
     * <p>
     * The config file can be present on the classpath (if the value starts with the {@code classpath:} prefix) or can
     * be loaded using the value as file path. The loaded configuration supersedes relevant properties passed to the
     * driver, e.g., {@link #USE_VOLATILE_STORAGE} or {@link #USE_INFERENCE}.
     * <p>
     * Note that the config applies only to embedded repositories created by the driver, repositories on a RDF4J server
     * to which the driver just connects must preexist and the configuration does not apply to them. The physical URI
     * specified in configuration must correspond to the URI of the repository in the config file, i.e., for memory
     * store, the repository ID must be the same. For a native store, the physical URI must be in the form {@code
     * /local-path/repositories/repository-id}, where {@code local-path} will be used for initialization of a local
     * {@link org.eclipse.rdf4j.repository.manager.RepositoryManager} and {@code repository-id} must again correspond to
     * the repository ID in the configuration file.
     */
    public static final String REPOSITORY_CONFIG = "cz.cvut.kbss.ontodriver.rdf4j.repository-config";

    /**
     * Number of attempts to reconnect to the repository in case of connection issues.
     * <p>
     * This setting applies only when using a remote rdf4j-server repository (including GraphDB). Applies also when
     * initial connection on startup is attempted.
     * <p>
     * Note that once connection is successful, the counter of failed attempts is reset.
     */
    public static final String RECONNECT_ATTEMPTS = "cz.cvut.kbss.ontodriver.rdf4j.reconnect-attempts";

    /**
     * Whether inferred assertion values are expected to exist in the default repository context.
     * <p>
     * Normally, RDF4J stores inferred statements in the context of the statement from which they were inferred.
     * However, if one uses SPIN rules or custom inference rules
     * <a href="https://rdf4j.org/documentation/programming/repository/#custom-inferencing">https://rdf4j.org/documentation/programming/repository/#custom-inferencing</a>,
     * their results will be stored in the default context. Setting this parameter to {@code true} will cause the
     * statement loading to use the default context for loading inferred assertions.
     * <p>
     * Note that GraphDB inference is handled automatically and does not need this parameter. However, setting it will
     * override the default automatic inference context resolution for GraphDB.
     * <p>
     * Defaults to {@code false}, i.e., inferred statements are expected in the same context as their causes.
     */
    public static final String INFERENCE_IN_DEFAULT_CONTEXT = "cz.cvut.kbss.ontodriver.rdf4j.inference-in-default-context";

    /**
     * Timeout for the underlying HTTP client to request a connection to the remote repository from the connection
     * pool.
     * <p>
     * RDF4J internally uses the Apache HTTP Client with a connection pool that is able to grow up to a configured
     * limit. When the pool is exhausted and the driver requests another connection, it may happen that no connection is
     * available will become available, possibly leading to a deadlock. This parameter sets a timeout for the connection
     * acquisition requests from the pool, so that the application is able to fail gracefully in case of too much load
     * and does not get stuck.
     * <p>
     * The value should be an integer corresponding to the number of milliseconds.
     * <p>
     * Applies only to instances connected to a RDF4J server.
     */
    public static final String CONNECTION_REQUEST_TIMEOUT = "cz.cvut.kbss.ontodriver.rdf4j.connection-request-timeout";

    /**
     * Maximum size of the underlying HTTP client connection pool.
     * <p>
     * RDF4J internally uses the Apache HTTP Client with a connection pool that is able to grow up to a configured
     * limit. This parameter allows to set this limit.
     */
    public static final String MAX_CONNECTION_POOL_SIZE = "cz.cvut.kbss.ontodriver.rdf4j.max-connections";

    private Rdf4jOntoDriverProperties() {
        throw new AssertionError();
    }
}
