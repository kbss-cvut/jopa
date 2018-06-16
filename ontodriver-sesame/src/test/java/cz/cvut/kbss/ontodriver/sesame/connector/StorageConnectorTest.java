/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
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
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.eclipse.rdf4j.sail.nativerdf.config.NativeStoreConfig;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.io.File;
import java.net.URI;
import java.nio.file.Files;
import java.util.Collection;

import static org.junit.Assert.*;

public class StorageConnectorTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private File repositoryFolder;

    private StorageConnector connector;

    @After
    public void tearDown() throws Exception {
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
    public void nonExistingParentFoldersAreCreatedWhenStorageIsInitialized() throws Exception {
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

    @Test(expected = RepositoryCreationException.class)
    public void invalidLocalRepositoryPathThrowsRepositoryCreationException() throws Exception {
        final URI invalidUri = URI
                .create("file://" + getProjectRootPath() + File.separator + "reps" + File.separator + "repositoryTest");
        final File parentDir = new File(getProjectRootPath() + File.separator + "reps");
        assertFalse(parentDir.exists());
        this.repositoryFolder = parentDir;
        new StorageConnector(TestUtils.createDriverConfig(invalidUri.toString()));
    }

    @Test
    public void connectorIsAbleToConnectToAlreadyInitializedLocalNativeStorage() throws Exception {
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
    public void unwrapReturnsItselfWhenClassMatches() throws Exception {
        createInMemoryConnector();
        assertSame(connector, connector.unwrap(StorageConnector.class));
    }

    @Test
    public void unwrapReturnsSesameRepository() throws Exception {
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

    @Test(expected = SesameDriverException.class)
    public void unwrapOfUnsupportedClassThrowsException() throws Exception {
        createInMemoryConnector();
        connector.unwrap(Boolean.class);
    }

    @Test
    public void setRepositoryReplacesOriginalInMemoryRepositoryWithSpecifiedOne() throws Exception {
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
    public void setRepositoryThrowsUnsupportedOperationWhenOriginalRepositoryIsNotInMemory() throws Exception {
        this.repositoryFolder = Files.createTempDirectory("sesame-storage-connector-test").toFile();
        connector = new StorageConnector(TestUtils.createDriverConfig(repositoryFolder
                .getAbsolutePath() + File.separator + "repositories" + File.separator + "test"));

        final Repository newRepository = new SailRepository(new MemoryStore());
        newRepository.initialize();
        try {
            thrown.expect(UnsupportedOperationException.class);
            thrown.expectMessage("Cannot replace repository which is not in-memory.");
            connector.setRepository(newRepository);
        } finally {
            newRepository.shutDown();
        }
    }

    @Test
    public void setRepositoryThrowsIllegalStateExceptionWhenConnectorIsInTransaction() throws Exception {
        createInMemoryConnector();
        connector.begin();
        final Repository newRepository = new SailRepository(new MemoryStore());
        newRepository.initialize();
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage("Cannot replace repository in transaction.");

        try {
            connector.setRepository(newRepository);
        } finally {
            newRepository.shutDown();
            connector.rollback();
        }
    }
}
