/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.RepositoryCreationException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.RepositoryNotFoundException;
import org.eclipse.rdf4j.model.Model;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.vocabulary.CONFIG;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.config.RepositoryConfig;
import org.eclipse.rdf4j.repository.config.RepositoryConfigException;
import org.eclipse.rdf4j.repository.config.RepositoryConfigSchema;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.repository.manager.RemoteRepositoryManager;
import org.eclipse.rdf4j.repository.manager.RepositoryManager;
import org.eclipse.rdf4j.repository.manager.RepositoryProvider;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.repository.sail.config.SailRepositoryConfig;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.Rio;
import org.eclipse.rdf4j.sail.config.SailImplConfig;
import org.eclipse.rdf4j.sail.inferencer.fc.SchemaCachingRDFSInferencer;
import org.eclipse.rdf4j.sail.inferencer.fc.config.SchemaCachingRDFSInferencerConfig;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.eclipse.rdf4j.sail.nativerdf.config.NativeStoreConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Optional;
import java.util.Set;

public class RepositoryConnectorInitializer {

    private static final Logger LOG = LoggerFactory.getLogger(RepositoryConnectorInitializer.class);

    private static final String[] KNOWN_REMOTE_SCHEMES = {"http", "https", "ftp"};
    private static final String LOCAL_NATIVE_REPO = "repositories/";
    private static final String FILE_SCHEME = "file";
    private static final String CLASSPATH_PREFIX = "classpath:";

    private final DriverConfiguration configuration;
    private final int maxReconnectAttempts;

    private RepositoryManager manager;
    private Repository repository;

    public RepositoryConnectorInitializer(DriverConfiguration configuration) throws Rdf4jDriverException {
        this.configuration = configuration;
        this.maxReconnectAttempts = resolveMaxReconnectAttempts();
    }

    private int resolveMaxReconnectAttempts() throws Rdf4jDriverException {
        try {
            final int attempts = configuration.isSet(Rdf4jConfigParam.RECONNECT_ATTEMPTS) ? Integer.parseInt(
                    configuration.getProperty(Rdf4jConfigParam.RECONNECT_ATTEMPTS)) :
                    Constants.DEFAULT_RECONNECT_ATTEMPTS_COUNT;
            if (attempts < 0) {
                throw invalidReconnectAttemptsConfig();
            }
            return attempts;
        } catch (NumberFormatException e) {
            throw invalidReconnectAttemptsConfig();
        }
    }

    private static Rdf4jDriverException invalidReconnectAttemptsConfig() {
        return new Rdf4jDriverException(
                "Invalid value of configuration parameter " + Rdf4jOntoDriverProperties.RECONNECT_ATTEMPTS +
                        ". Must be a non-negative integer.");
    }

    public void initializeRepository() throws Rdf4jDriverException {
        final URI serverUri = configuration.getStorageProperties().getPhysicalURI();
        LOG.debug("Initializing connector to repository at {}", serverUri);
        try {
            final boolean isRemote = isRemoteRepository(serverUri);
            if (isRemote) {
                this.repository = connectToRemoteRepository(serverUri.toString());
            } else {
                this.repository = createLocalRepository();
            }
            verifyRepositoryCreated(serverUri, isRemote);
            repository.init();
        } catch (RepositoryException | RepositoryConfigException e) {
            throw new Rdf4jDriverException("Failed to acquire RDF4J repository connection.", e);
        }
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

    private Repository connectToRemoteRepository(String repoUri) {
        this.manager = RepositoryProvider.getRepositoryManagerOfRepository(repoUri);
        final RemoteRepositoryManager remoteManager = (RemoteRepositoryManager) manager;
        final String username = configuration.getStorageProperties().getUsername();
        if (username != null) {
            final String password = configuration.getStorageProperties().getPassword();
            remoteManager.setUsernameAndPassword(username, password);
        }
        return connectToRemote(repoUri, 1);
    }

    private Repository connectToRemote(String repoUri, int attempts) {
        try {
            return new RemoteRepositoryWrapper((HTTPRepository) manager.getRepository(RepositoryProvider.getRepositoryIdOfRepository(repoUri)), configuration);
        } catch (RepositoryException e) {
            if (attempts < maxReconnectAttempts) {
                LOG.warn("Unable to connect to repository {}. Error is: {}. Retrying...", repoUri, e.getMessage());
                return connectToRemote(repoUri, attempts + 1);
            }
            LOG.error("Threshold of failed connection attempts reached, throwing exception.");
            throw e;
        }
    }

    private Repository createLocalRepository() {
        if (configuration.isSet(Rdf4jConfigParam.REPOSITORY_CONFIG)) {
            return createRepositoryFromConfig();
        }
        final URI localUri = configuration.getStorageProperties().getPhysicalURI();
        if (!isFileUri(localUri) && configuration.is(Rdf4jConfigParam.USE_VOLATILE_STORAGE)) {
            return createInMemoryRepository();
        } else {
            return createNativeRepository(configuration, localUri.toString());
        }
    }

    private Repository createRepositoryFromConfig() {
        LOG.trace("Creating local repository from repository config file.");
        final RepositoryConfig repoConfig = loadRepositoryConfig();
        this.manager = RepositoryProvider.getRepositoryManager(getRepositoryManagerBaseDir().orElse(""));
        manager.addRepositoryConfig(repoConfig);
        return manager.getRepository(getRepositoryId());
    }

    @SuppressWarnings("deprecated")
    private RepositoryConfig loadRepositoryConfig() {
        try (final InputStream is = getConfigFileContent()) {
            final Model configModel = Rio.parse(is, "", RDFFormat.TURTLE);
            Set<Resource> resources =
                    configModel.filter(null, RDF.TYPE, CONFIG.Rep.Repository).subjects();
            if (resources.isEmpty()) {
                // Support for legacy repository configuration vocabulary.
                // https://rdf4j.org/documentation/reference/configuration/#migrating-old-configurations
                resources = configModel.filter(null, RDF.TYPE, RepositoryConfigSchema.REPOSITORY).subjects();
            }
            assert resources.size() == 1;
            return RepositoryConfig.create(configModel, resources.iterator().next());
        } catch (IOException e) {
            throw new RepositoryCreationException("Unable to create repository from the specified configuration.", e);
        }
    }

    private InputStream getConfigFileContent() {
        final String configPath = configuration.getProperty(Rdf4jConfigParam.REPOSITORY_CONFIG);
        LOG.trace("Loading repository configuration file content from {}.", configPath);
        if (configPath.startsWith(CLASSPATH_PREFIX)) {
            final InputStream is =
                    getClass().getClassLoader().getResourceAsStream(configPath.substring(CLASSPATH_PREFIX.length()));
            if (is == null) {
                throw new RepositoryCreationException(
                        "Unable to find repository configuration file on classpath location " + configPath);
            }
            return is;
        } else {
            try {
                return new FileInputStream(configPath);
            } catch (FileNotFoundException e) {
                throw new RepositoryCreationException("Unable to find repository configuration file at " + configPath,
                        e);
            }
        }
    }

    private Optional<String> getRepositoryManagerBaseDir() {
        final String physicalUri = configuration.getStorageProperties().getPhysicalURI().toString();
        final String[] tmp = physicalUri.split(LOCAL_NATIVE_REPO);
        return tmp.length == 2 ? Optional.of(tmp[0]) : Optional.empty();
    }

    private String getRepositoryId() {
        final String physicalUri = configuration.getStorageProperties().getPhysicalURI().toString();
        final String[] tmp = physicalUri.split(LOCAL_NATIVE_REPO);
        if (tmp.length != 2) {
            return physicalUri;
        }
        String repoId = tmp[1];
        // Get rid of the trailing slash if necessary
        return repoId.charAt(repoId.length() - 1) == '/' ? repoId.substring(0, repoId.length() - 1) : repoId;
    }

    private static boolean isFileUri(URI uri) {
        return uri.getScheme() != null && uri.getScheme().equals(FILE_SCHEME);
    }

    /**
     * Creates a local in-memory RDF4J repository which is disposed of when the VM shuts down.
     */
    private Repository createInMemoryRepository() {
        LOG.trace("Creating local in-memory repository.");
        final MemoryStore ms = new MemoryStore();
        if (configuration.is(Rdf4jConfigParam.USE_INFERENCE)) {
            return new SailRepository(new SchemaCachingRDFSInferencer(ms));
        } else {
            return new SailRepository(ms);
        }
    }

    /**
     * Creates native repository.
     * <p>
     * This kind of repository stores data in files and is persistent after the VM shuts down.
     */
    private Repository createNativeRepository(DriverConfiguration configuration, String localUri) {
        LOG.trace("Creating local native repository at " + localUri);
        validateNativeStorePath(localUri);
        try {
            this.manager = RepositoryProvider.getRepositoryManagerOfRepository(localUri);
            final String repoId = getRepositoryId();
            final RepositoryConfig cfg = createLocalNativeRepositoryConfig(repoId, configuration);
            manager.addRepositoryConfig(cfg);
            return manager.getRepository(repoId);
        } catch (RepositoryConfigException | RepositoryException e) {
            throw new RepositoryCreationException("Unable to create local repository at " + localUri, e);
        }
    }

    private static void validateNativeStorePath(String path) {
        if (path.split(LOCAL_NATIVE_REPO).length != 2) {
            throw new RepositoryCreationException(
                    "Unsupported local RDF4J repository path. Expected file://path/repositories/id but got " +
                            path);
        }
    }

    private static RepositoryConfig createLocalNativeRepositoryConfig(String repoId,
                                                                      DriverConfiguration configuration) {
        SailImplConfig backend = new NativeStoreConfig();
        if (configuration.is(Rdf4jConfigParam.USE_INFERENCE)) {
            backend = new SchemaCachingRDFSInferencerConfig(backend);
        }
        final SailRepositoryConfig repoType = new SailRepositoryConfig(backend);
        return new RepositoryConfig(repoId, repoType);
    }

    private void verifyRepositoryCreated(URI serverUri, boolean isRemote) {
        if (repository == null) {
            if (isRemote) {
                throw new RepositoryNotFoundException("Unable to reach repository at " + serverUri);
            } else {
                throw new RepositoryCreationException("Unable to create local repository at " + serverUri);
            }
        }
    }

    public RepositoryManager getManager() {
        return manager;
    }

    public Repository getRepository() {
        return repository;
    }

    public int getMaxReconnectAttempts() {
        return maxReconnectAttempts;
    }
}
