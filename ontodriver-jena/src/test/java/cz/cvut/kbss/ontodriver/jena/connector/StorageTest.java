package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.RDF;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.io.File;
import java.nio.file.Files;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class StorageTest extends StorageTestUtil {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void createInitializesMemoryStorageForMemoryConfiguration() {
        final DriverConfiguration config = createConfiguration("test:uri");
        config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof MemoryStorage);
        assertNotNull(result.getDataset());
    }

    @Test
    public void createInitializesFileStorageForFileConfiguration() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final DriverConfiguration config = createConfiguration(file.getAbsolutePath());
        config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.FILE);
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof FileStorage);
    }

    @Test
    public void createThrowsInitializationExceptionForUnknownStorageType() {
        thrown.expect(OntoDriverInitializationException.class);
        thrown.expectMessage(containsString("Unsupported storage type"));
        final DriverConfiguration config = createConfiguration("test:uri");
        config.setProperty(JenaConfigParam.STORAGE_TYPE, "dontKnowThisOne");
        Storage.create(config);
    }

    @Test
    public void createCreatesInMemoryByDefault() {
        final DriverConfiguration config = createConfiguration("test:uri");
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof MemoryStorage);
    }

    @Test
    public void getDefaultGraphReturnsUnionModelWhenConfiguredTo() {
        final DriverConfiguration config = createConfiguration("test:uri");
        config.setProperty(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION, Boolean.toString(true));
        final Storage result = Storage.create(config);
        final String ctx = "test";
        result.dataset.getNamedModel(ctx).add(ResourceFactory
                .createStatement(ResourceFactory.createResource(), RDF.type, ResourceFactory.createResource(
                        Generator.generateUri().toString())));
        assertFalse(result.getNamedGraph(ctx).isEmpty());
        assertFalse(result.getDefaultGraph().isEmpty());
    }
}