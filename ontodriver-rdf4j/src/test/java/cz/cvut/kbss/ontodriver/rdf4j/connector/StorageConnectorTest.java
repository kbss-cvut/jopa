/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.environment.TestUtils;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.RepositoryCreationException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.config.RepositoryConfig;
import org.eclipse.rdf4j.repository.manager.RepositoryManager;
import org.eclipse.rdf4j.repository.manager.RepositoryProvider;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.repository.sail.config.SailRepositoryConfig;
import org.eclipse.rdf4j.sail.config.SailImplConfig;
import org.eclipse.rdf4j.sail.inferencer.fc.SchemaCachingRDFSInferencer;
import org.eclipse.rdf4j.sail.lucene.LuceneSail;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.eclipse.rdf4j.sail.nativerdf.config.NativeStoreConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class StorageConnectorTest {

    private File repositoryFolder;

    private StorageConnector sut;

    @AfterEach
    void tearDown() throws Exception {
        if (sut != null && sut.isOpen()) {
            sut.close();
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
        final URI fileUri = Paths.get(projectRootPath + File.separator + "internal" + File.separator + "folder" +
                File.separator + "repositories" + File.separator + "repositoryTest").toUri();
        final File parentDir = new File(projectRootPath + File.separator + "internal");
        assertFalse(parentDir.exists());
        this.repositoryFolder = parentDir;
        this.sut = createSut(TestUtils.createDriverConfig(fileUri.toString()));
        assertTrue(parentDir.exists());
        final File repositoryDir = new File(fileUri);
        assertTrue(repositoryDir.exists());
    }

    private String getProjectRootPath() {
        String projectRootPath = new File("pom.xml").getAbsolutePath();
        projectRootPath = projectRootPath.substring(0, projectRootPath.lastIndexOf(File.separator));
        return projectRootPath;
    }

    private static StorageConnector createSut(DriverConfiguration config) throws Rdf4jDriverException {
        final StorageConnector connector = new StorageConnector(config);
        connector.initializeRepository();
        return connector;
    }

    @Test
    void invalidLocalRepositoryPathThrowsRepositoryCreationException() {
        final URI invalidUri = Paths.get(getProjectRootPath() + File.separator + "reps" + File.separator + "repositoryTest")
                .toUri();
        final File parentDir = new File(getProjectRootPath() + File.separator + "reps");
        assertFalse(parentDir.exists());
        this.repositoryFolder = parentDir;
        assertThrows(RepositoryCreationException.class,
                () -> createSut(TestUtils.createDriverConfig(invalidUri.toString())));
    }

    @Test
    void connectorIsAbleToConnectToAlreadyInitializedLocalNativeStorage() throws Exception {
        final String repoId = "repositoryTest";
        final URI repoUri = Paths.get(getProjectRootPath() + File.separator + "repositories" + File.separator + repoId)
                .toUri();
        this.repositoryFolder = new File(getProjectRootPath() + File.separator + "repositories");
        SailImplConfig backend = new NativeStoreConfig();
        final SailRepositoryConfig repoType = new SailRepositoryConfig(backend);
        final RepositoryConfig config = new RepositoryConfig(repoId, repoType);
        final RepositoryManager repoManager = RepositoryProvider.getRepositoryManagerOfRepository(repoUri.toString());
        repoManager.addRepositoryConfig(config);
        repoManager.getRepository(repoId);

        this.sut = createSut(TestUtils.createDriverConfig(repoUri.toString()));
        assertTrue(sut.isOpen());
        sut.close();
    }

    @Test
    void unwrapReturnsItselfWhenClassMatches() throws Exception {
        createInMemoryConnector();
        assertSame(sut, sut.unwrap(StorageConnector.class));
    }

    @Test
    void unwrapReturnsUnderlyingRepository() throws Exception {
        createInMemoryConnector();
        final Repository repo = sut.unwrap(Repository.class);
        assertNotNull(repo);
        assertTrue(repo.isInitialized());
    }

    private void createInMemoryConnector() throws Rdf4jDriverException {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        this.sut = createSut(conf);
    }

    @Test
    void unwrapOfUnsupportedClassThrowsException() throws Exception {
        createInMemoryConnector();
        assertThrows(Rdf4jDriverException.class, () -> sut.unwrap(Boolean.class));
    }

    @Test
    void setRepositoryReplacesOriginalInMemoryRepositoryWithSpecifiedOne() throws Exception {
        createInMemoryConnector();
        final Repository newRepository = new SailRepository(new MemoryStore());
        Generator.initTestData(newRepository);
        sut.setRepository(newRepository);
        try (RepositoryConnection expectedConn = newRepository.getConnection()) {
            try (RepositoryConnection actualConn = sut.acquireConnection()) {
                assertEquals(expectedConn.getStatements(null, null, null, false).stream().toList(),
                        actualConn.getStatements(null, null, null, false).stream().toList());
            }
        }
    }

    @Test
    void setRepositoryThrowsUnsupportedOperationWhenOriginalRepositoryIsNotInMemory() throws Exception {
        this.repositoryFolder = Files.createTempDirectory("rdf4j-storage-connector-test").toFile();
        this.sut = createSut(TestUtils.createDriverConfig(Paths.get(repositoryFolder
                .getAbsolutePath() + File.separator + "repositories" + File.separator + "test").toUri().toString()));

        final Repository newRepository = new SailRepository(new MemoryStore());
        try {
            final UnsupportedOperationException result =
                    assertThrows(UnsupportedOperationException.class, () -> sut.setRepository(newRepository));
            assertEquals("Cannot replace repository which is not in-memory.", result.getMessage());
        } finally {
            newRepository.shutDown();
        }
    }

    @Test
    void initializationLoadsRepositoryConfigurationFromFileOnClasspathAndCreatesRepo() throws Exception {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.REPOSITORY_CONFIG, "classpath:repo-configs/memory-rdfs.ttl");
        this.sut = createSut(conf);
        final Repository repo = sut.unwrap(Repository.class);
        assertInstanceOf(SailRepository.class, repo);
        assertInstanceOf(SchemaCachingRDFSInferencer.class, ((SailRepository) repo).getSail());
    }

    @Test
    void initializationSupportsLegacyRepositoryConfigurationVocabulary() throws Exception {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.REPOSITORY_CONFIG, "classpath:repo-configs/memory-rdfs-legacy.ttl");
        this.sut = createSut(conf);
        final Repository repo = sut.unwrap(Repository.class);
        assertInstanceOf(SailRepository.class, repo);
        assertInstanceOf(SchemaCachingRDFSInferencer.class, ((SailRepository) repo).getSail());
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
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.REPOSITORY_CONFIG, file.toString());
        this.sut = createSut(conf);
        final Repository repo = sut.unwrap(Repository.class);
        assertInstanceOf(SailRepository.class, repo);
        assertInstanceOf(SchemaCachingRDFSInferencer.class, ((SailRepository) repo).getSail());
    }

    @Test
    void initializationThrowsRepositoryCreationExceptionWhenRepositoryConfigurationFileIsNotFoundOnClasspath() {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.REPOSITORY_CONFIG, "classpath:repo-configs/memory-rdfs-unknown.ttl");
        final RepositoryCreationException result =
                assertThrows(RepositoryCreationException.class, () -> createSut(conf));
        assertThat(result.getMessage(), containsString("repo-configs/memory-rdfs-unknown.ttl"));
    }

    @Test
    void initializationThrowsRepositoryCreationExceptionWhenRepositoryConfigurationFileIsNotFound() {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.REPOSITORY_CONFIG, "/tmp/memory-rdfs-unknown.ttl");
        final RepositoryCreationException result =
                assertThrows(RepositoryCreationException.class, () -> createSut(conf));
        assertThat(result.getMessage(), containsString("/tmp/memory-rdfs-unknown.ttl"));
    }

    @Test
    void initializationLoadsRepositoryConfigurationFromFileAndCreatesNativeRepo() throws Exception {
        final Path serverDir = Files.createTempDirectory("rdf4j-config-test");
        this.repositoryFolder = serverDir.toFile();
        final String physicalUri = Paths.get(serverDir.toAbsolutePath()
                .toString(), File.separator + "repositories" + File.separator + "native-lucene").toUri().toString();
        final DriverConfiguration conf = TestUtils.createDriverConfig(physicalUri);
        conf.setProperty(Rdf4jConfigParam.REPOSITORY_CONFIG, "classpath:repo-configs/native-lucene.ttl");
        this.sut = createSut(conf);
        final Repository repo = sut.unwrap(Repository.class);
        assertInstanceOf(SailRepository.class, repo);
        assertInstanceOf(LuceneSail.class, ((SailRepository) repo).getSail());
        final File repoDir = new File(URI.create(physicalUri));
        assertTrue(repoDir.exists());
    }

    @Test
    void initializationThrowsIllegalArgumentDriverExceptionWhenReconnectAttemptsIsNotANumber() {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.RECONNECT_ATTEMPTS, "not-a-number");
        conf.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        assertThrows(IllegalArgumentException.class, () -> createSut(conf));
    }

    @Test
    void initializationThrowsRdf4jDriverExceptionWhenReconnectAttemptsIsNegative() {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.RECONNECT_ATTEMPTS, "-1");
        conf.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        assertThrows(Rdf4jDriverException.class, () -> createSut(conf));
    }

    @Test
    void getConnectionRetriesOnErrorConfiguredNumberOfTimes() throws Exception {
        final int attempts = 3;
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        conf.setProperty(Rdf4jConfigParam.RECONNECT_ATTEMPTS, Integer.toString(attempts));
        this.sut = createSut(conf);
        final Repository repoMock = mock(Repository.class);
        final Field repoField = StorageConnector.class.getDeclaredField("repository");
        repoField.setAccessible(true);
        ((Repository) repoField.get(sut)).shutDown();
        repoField.set(sut, repoMock);
        when(repoMock.getConnection()).thenThrow(RepositoryException.class);
        when(repoMock.isInitialized()).thenReturn(true);

        assertThrows(Rdf4jDriverException.class, () -> sut.acquireConnection());
        verify(repoMock, times(attempts)).getConnection();
    }
}
