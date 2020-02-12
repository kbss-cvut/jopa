/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import cz.cvut.kbss.ontodriver.sesame.environment.TestUtils;
import cz.cvut.kbss.ontodriver.sesame.exceptions.RepositoryCreationException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.common.iteration.Iterations;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.config.RepositoryConfig;
import org.eclipse.rdf4j.repository.manager.RepositoryManager;
import org.eclipse.rdf4j.repository.manager.RepositoryProvider;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.repository.sail.config.SailRepositoryConfig;
import org.eclipse.rdf4j.sail.config.SailImplConfig;
import org.eclipse.rdf4j.sail.inferencer.fc.ForwardChainingRDFSInferencer;
import org.eclipse.rdf4j.sail.lucene.LuceneSail;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.eclipse.rdf4j.sail.nativerdf.config.NativeStoreConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Comparator;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class StorageConnectorTest {

    private File repositoryFolder;

    private StorageConnector connector;

    @AfterEach
    void tearDown() throws Exception {
        if (connector != null && connector.isOpen()) {
            connector.close();
        }
        if (repositoryFolder != null && repositoryFolder.exists()) {
            deleteRecursive(repositoryFolder);
        }
    }

    private void deleteRecursive(File path) {
        if (path.isDirectory()) {
            if (path.listFiles() != null) {
                for (File f : path.listFiles()) {
                    deleteRecursive(f);
                }
            }
        }
        path.delete();
    }

    @Test
    void nonExistingParentFoldersAreCreatedWhenStorageIsInitialized() throws Exception {
        final String projectRootPath = getProjectRootPath();
        final URI fileUri = URI
                .create("file://" + projectRootPath + File.separator + "internal" + File.separator + "folder" +
                        File.separator +
                        "repositories" + File.separator + "repositoryTest");
        final File parentDir = new File(projectRootPath + File.separator + "internal");
        assertFalse(parentDir.exists());
        this.repositoryFolder = parentDir;
        connector = new StorageConnector(TestUtils.createDriverConfig(fileUri.toString()));
        assertTrue(parentDir.exists());
        final File repositoryDir = new File(fileUri);
        assertTrue(repositoryDir.exists());
    }

    private String getProjectRootPath() {
        String projectRootPath = new File("pom.xml").getAbsolutePath();
        projectRootPath = projectRootPath.substring(0, projectRootPath.lastIndexOf(File.separator));
        return projectRootPath;
    }

    @Test
    void invalidLocalRepositoryPathThrowsRepositoryCreationException() {
        final URI invalidUri = URI
                .create("file://" + getProjectRootPath() + File.separator + "reps" + File.separator + "repositoryTest");
        final File parentDir = new File(getProjectRootPath() + File.separator + "reps");
        assertFalse(parentDir.exists());
        this.repositoryFolder = parentDir;
        assertThrows(RepositoryCreationException.class,
                () -> new StorageConnector(TestUtils.createDriverConfig(invalidUri.toString())));
    }

    @Test
    void connectorIsAbleToConnectToAlreadyInitializedLocalNativeStorage() throws Exception {
        final String repoId = "repositoryTest";
        final URI repoUri = URI
                .create("file://" + getProjectRootPath() + File.separator + "repositories" + File.separator + repoId);
        this.repositoryFolder = new File(getProjectRootPath() + File.separator + "repositories");
        SailImplConfig backend = new NativeStoreConfig();
        final SailRepositoryConfig repoType = new SailRepositoryConfig(backend);
        final RepositoryConfig config = new RepositoryConfig(repoId, repoType);
        final RepositoryManager repoManager = RepositoryProvider.getRepositoryManagerOfRepository(repoUri.toString());
        repoManager.addRepositoryConfig(config);
        final Repository repo = repoManager.getRepository(repoId);
        repo.initialize();

        final StorageConnector connector = new StorageConnector(TestUtils.createDriverConfig(repoUri.toString()));
        assertTrue(connector.isOpen());
        connector.close();
    }

    @Test
    void unwrapReturnsItselfWhenClassMatches() throws Exception {
        createInMemoryConnector();
        assertSame(connector, connector.unwrap(StorageConnector.class));
    }

    @Test
    void unwrapReturnsSesameRepository() throws Exception {
        createInMemoryConnector();
        final Repository repo = connector.unwrap(Repository.class);
        assertNotNull(repo);
        assertTrue(repo.isInitialized());
    }

    private void createInMemoryConnector() throws SesameDriverException {
        final DriverConfiguration conf = TestUtils.createDriverConfig("urn:test");
        conf.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        this.connector = new StorageConnector(conf);
    }

    @Test
    void unwrapOfUnsupportedClassThrowsException() throws Exception {
        createInMemoryConnector();
        assertThrows(SesameDriverException.class, () -> connector.unwrap(Boolean.class));
    }

    @Test
    void setRepositoryReplacesOriginalInMemoryRepositoryWithSpecifiedOne() throws Exception {
        createInMemoryConnector();
        final Repository newRepository = new SailRepository(new MemoryStore());
        newRepository.initialize();
        Generator.initTestData(newRepository);
        connector.setRepository(newRepository);
        final Collection<Statement> content = connector.findStatements(null, null, null, false);
        try (RepositoryConnection conn = newRepository.getConnection()) {
            assertEquals(Iterations.asList(conn.getStatements(null, null, null, false)), content);
        }
    }

    @Test
    void setRepositoryThrowsUnsupportedOperationWhenOriginalRepositoryIsNotInMemory() throws Exception {
        this.repositoryFolder = Files.createTempDirectory("sesame-storage-connector-test").toFile();
        connector = new StorageConnector(TestUtils.createDriverConfig(repositoryFolder
                .getAbsolutePath() + File.separator + "repositories" + File.separator + "test"));

        final Repository newRepository = new SailRepository(new MemoryStore());
        newRepository.initialize();
        try {
            final UnsupportedOperationException result =
                    assertThrows(UnsupportedOperationException.class, () -> connector.setRepository(newRepository));
            assertEquals("Cannot replace repository which is not in-memory.", result.getMessage());
        } finally {
            newRepository.shutDown();
        }
    }

    @Test
    void setRepositoryThrowsIllegalStateExceptionWhenConnectorIsInTransaction() throws Exception {
        createInMemoryConnector();
        connector.begin();
        final Repository newRepository = new SailRepository(new MemoryStore());
        newRepository.initialize();

        try {
            final IllegalStateException result =
                    assertThrows(IllegalStateException.class, () -> connector.setRepository(newRepository));
            assertEquals("Cannot replace repository in transaction.", result.getMessage());
        } finally {
            newRepository.shutDown();
            connector.rollback();
        }
    }

    @Test
    void initializationLoadsRepositoryConfigurationFromFileOnClasspathAndCreatesRepo() throws Exception {
        final DriverConfiguration conf = TestUtils.createDriverConfig("urn:test");
        conf.setProperty(SesameConfigParam.REPOSITORY_CONFIG, "classpath:repo-configs/memory-rdfs.ttl");
        this.connector = new StorageConnector(conf);
        final Repository repo = connector.unwrap(Repository.class);
        assertTrue(repo instanceof SailRepository);
        assertTrue(((SailRepository) repo).getSail() instanceof ForwardChainingRDFSInferencer);
    }

    @Test
    void initializationLoadsRepositoryConfigurationFromFileWithAbsolutePathAndCreatesRepo() throws Exception {
        final Path file = Files.createTempFile("memory-rdfs", ".ttl");
        try (final BufferedReader in = new BufferedReader(
                new InputStreamReader(
                        getClass().getClassLoader().getResourceAsStream("repo-configs/memory-rdfs.ttl")))) {
            final String content = in.lines().collect(Collectors.joining("\n"));
            Files.write(file, content.getBytes());
        }
        file.toFile().deleteOnExit();
        final DriverConfiguration conf = TestUtils.createDriverConfig("urn:test");
        conf.setProperty(SesameConfigParam.REPOSITORY_CONFIG, file.toString());
        this.connector = new StorageConnector(conf);
        final Repository repo = connector.unwrap(Repository.class);
        assertTrue(repo instanceof SailRepository);
        assertTrue(((SailRepository) repo).getSail() instanceof ForwardChainingRDFSInferencer);
    }

    @Test
    void initializationThrowsRepositoryCreationExceptionWhenRepositoryConfigurationFileIsNotFoundOnClasspath() {
        final DriverConfiguration conf = TestUtils.createDriverConfig("urn:test");
        conf.setProperty(SesameConfigParam.REPOSITORY_CONFIG, "classpath:repo-configs/memory-rdfs-unknown.ttl");
        final RepositoryCreationException result =
                assertThrows(RepositoryCreationException.class, () -> new StorageConnector(conf));
        assertThat(result.getMessage(), containsString("repo-configs/memory-rdfs-unknown.ttl"));
    }

    @Test
    void initializationThrowsRepositoryCreationExceptionWhenRepositoryConfigurationFileIsNotFound() {
        final DriverConfiguration conf = TestUtils.createDriverConfig("urn:test");
        conf.setProperty(SesameConfigParam.REPOSITORY_CONFIG, "/tmp/memory-rdfs-unknown.ttl");
        final RepositoryCreationException result =
                assertThrows(RepositoryCreationException.class, () -> new StorageConnector(conf));
        assertThat(result.getMessage(), containsString("/tmp/memory-rdfs-unknown.ttl"));
    }

    @Test
    void initializationLoadsRepositoryConfigurationFromFileAndCreatesNativeRepo() throws Exception {
        final Path serverDir = Files.createTempDirectory("sesame-config-test");
        final String physicalUri = serverDir.toString() + "/repositories/native-lucene";
        try {
            final DriverConfiguration conf = TestUtils.createDriverConfig(physicalUri);
            conf.setProperty(SesameConfigParam.REPOSITORY_CONFIG, "classpath:repo-configs/native-lucene.ttl");
            this.connector = new StorageConnector(conf);
            final Repository repo = connector.unwrap(Repository.class);
            assertTrue(repo instanceof SailRepository);
            assertTrue(((SailRepository) repo).getSail() instanceof LuceneSail);
            final File repoDir = new File(physicalUri);
            assertTrue(repoDir.exists());
        } finally {
            Files.walk(serverDir)
                 .sorted(Comparator.reverseOrder())
                 .map(Path::toFile)
                 .forEach(File::delete);
        }
    }
}
