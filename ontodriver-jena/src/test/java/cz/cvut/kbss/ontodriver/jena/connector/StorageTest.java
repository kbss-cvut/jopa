package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.io.File;
import java.nio.file.Files;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class StorageTest extends StorageTestBase {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void createInitializesMemoryStorageForMemoryConfiguration() {
        final Configuration config = createConfiguration("test:uri");
        config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof MemoryStorage);
    }

    @Test
    public void createInitializesFileStorageForFileConfiguration() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final Configuration config = createConfiguration(file.getAbsolutePath());
        config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.FILE);
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof FileStorage);
    }

    @Test
    public void createThrowsInitializationExceptionForUnknownStorageType() {
        thrown.expect(OntoDriverInitializationException.class);
        thrown.expectMessage(containsString("Unsupported storage type"));
        final Configuration config = createConfiguration("test:uri");
        config.setProperty(JenaConfigParam.STORAGE_TYPE, "dontKnowThisOne");
        Storage.create(config);
    }

    @Test
    public void createCreatesInMemoryByDefault() {
        final Configuration config = createConfiguration("test:uri");
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof MemoryStorage);
    }
}