package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.sesame.SesameDataSource;
import cz.cvut.kbss.ontodriver.sesame.exceptions.RepositoryCreationException;
import org.junit.After;
import org.junit.Test;
import org.openrdf.repository.Repository;
import org.openrdf.repository.config.RepositoryConfig;
import org.openrdf.repository.manager.RepositoryManager;
import org.openrdf.repository.manager.RepositoryProvider;
import org.openrdf.repository.sail.config.SailRepositoryConfig;
import org.openrdf.sail.config.SailImplConfig;
import org.openrdf.sail.nativerdf.config.NativeStoreConfig;

import java.io.File;
import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
        connector = new StorageConnector(storageProperties, Collections.emptyMap());
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
        new StorageConnector(storageProperties, Collections.emptyMap());
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
        final RepositoryManager repoManager = RepositoryProvider
                .getRepositoryManagerOfRepository(repoUri.toASCIIString());
        repoManager.addRepositoryConfig(config);
        final Repository repo = repoManager.getRepository(repoId);
        repo.initialize();
        final OntologyStorageProperties storageProperties = OntologyStorageProperties.driver(DRIVER)
                                                                                     .physicalUri(repoUri).build();

        final StorageConnector connector = new StorageConnector(storageProperties, Collections.emptyMap());
        assertTrue(connector.isOpen());
        connector.close();
    }
}
