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

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.sesame.SesameDataSource;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.exceptions.RepositoryCreationException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.config.RepositoryConfig;
import org.eclipse.rdf4j.repository.manager.RepositoryManager;
import org.eclipse.rdf4j.repository.manager.RepositoryProvider;
import org.eclipse.rdf4j.repository.sail.config.SailRepositoryConfig;
import org.eclipse.rdf4j.sail.config.SailImplConfig;
import org.eclipse.rdf4j.sail.nativerdf.config.NativeStoreConfig;
import org.junit.After;
import org.junit.Test;

import java.io.File;
import java.net.URI;

import static org.junit.Assert.*;

public class StorageConnectorTest {

    private static final String DRIVER = SesameDataSource.class.getName();

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
        final OntologyStorageProperties storageProperties = OntologyStorageProperties.driver(DRIVER)
                                                                                     .physicalUri(fileUri).build();
        connector = new StorageConnector(new Configuration(storageProperties));
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
        final OntologyStorageProperties storageProperties = OntologyStorageProperties.driver(DRIVER)
                                                                                     .physicalUri(invalidUri).build();
        new StorageConnector(new Configuration(storageProperties));
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
        final OntologyStorageProperties storageProperties = OntologyStorageProperties.driver(DRIVER)
                                                                                     .physicalUri(repoUri).build();

        final StorageConnector connector = new StorageConnector(new Configuration(storageProperties));
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
        final Configuration conf = new Configuration(
                OntologyStorageProperties.driver(DRIVER).physicalUri("test").build());
        conf.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        this.connector = new StorageConnector(conf);
    }

    @Test(expected = SesameDriverException.class)
    public void unwrapOfUnsupportedClassThrowsException() throws Exception {
        createInMemoryConnector();
        connector.unwrap(Boolean.class);
    }
}
