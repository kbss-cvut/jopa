package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;
import org.junit.Test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.util.List;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class FileStorageTest extends StorageTestBase {

    @Test
    public void initializesStorageByReadingFileWithSingleGraph() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final Model model = generateModel();
        try (final BufferedWriter writer = new BufferedWriter(new FileWriter(file))) {
            model.write(writer, "TURTLE");
        }

        final Storage result = new FileStorage(createConfiguration(file.getAbsolutePath()));
        result.initialize();
        final Dataset dataset = result.getDataset();
        assertNotNull(dataset);
        assertFalse(dataset.getDefaultModel().isEmpty());
    }

    private Model generateModel() {
        final Model model = ModelFactory.createDefaultModel();
        final Resource resource = model.createResource("test:example");
        resource.addProperty(RDF.type, model.createResource("test:type"));
        return model;
    }

    @Test
    public void initializesStorageFromNonexistentFileByCreatingIt() {
        final String filePath = System.getProperty("java.io.tmpdir") + File.separator + "jena-test.ttl";
        final Storage result = new FileStorage(createConfiguration(filePath));
        result.initialize();
        final Dataset dataset = result.getDataset();
        assertNotNull(dataset);

        final File f = new File(filePath);
        assertTrue(f.exists());
        f.deleteOnExit();
    }

    @Test
    public void writeChangesOutputsChangesIntoTargetFile() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final Storage storage = new FileStorage(createConfiguration(file.getAbsolutePath()));
        storage.initialize();
        final Dataset dataset = storage.getDataset();
        final Model m = dataset.getDefaultModel();
        dataset.begin(ReadWrite.WRITE);
        final String rName = "test:example";
        final String rValue = "test:type";
        final Resource resource = m.createResource(rName);
        resource.addProperty(RDF.type, m.createResource(rValue));
        dataset.commit();
        storage.writeChanges();

        final List<String> lines = Files.readAllLines(file.toPath());
        assertFalse(lines.isEmpty());
        final String str = String.join("\n", lines);
        assertThat(str, containsString(rName));
        assertThat(str, containsString(rValue));
    }
}