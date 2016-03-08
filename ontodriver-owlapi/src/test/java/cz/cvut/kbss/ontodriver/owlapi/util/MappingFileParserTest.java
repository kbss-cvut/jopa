package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiOntoDriverProperties;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.exception.MappingFileParserException;
import org.junit.Test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.net.URI;
import java.util.*;

import static cz.cvut.kbss.ontodriver.owlapi.config.OwlapiOntoDriverProperties.IRI_MAPPING_DELIMITER;
import static cz.cvut.kbss.ontodriver.owlapi.config.OwlapiOntoDriverProperties.MAPPING_FILE_LOCATION;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class MappingFileParserTest {

    @Test(expected = MappingFileParserException.class)
    public void throwsExceptionWhenMappingFileCannotBeFound() throws Exception {
        final String someFilePath = "thisFile/does/not/exist";
        new MappingFileParser(Collections.singletonMap(MAPPING_FILE_LOCATION, someFilePath));
    }

    @Test
    public void mappingFileParserAlsoAcceptsURIAsMappingFilePath() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " " +
                OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER + " ./file.owl";
        final File mappingFile = createMappingFile(content);
        final Map<URI, URI> mappings = new MappingFileParser(
                Collections.singletonMap(MAPPING_FILE_LOCATION, mappingFile.toURI().toString())).getMappings();
        assertEquals(1, mappings.size());
    }

    @Test
    public void usesDefaultDelimiterWhenNoneIsProvided() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " " +
                OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER + " ./file.owl";
        final File mappingFile = createMappingFile(content);
        final Map<URI, URI> mappings = new MappingFileParser(
                Collections.singletonMap(MAPPING_FILE_LOCATION, mappingFile.getPath())).getMappings();
        assertEquals(1, mappings.size());
        assertTrue(mappings.get(URI.create(ontUri)).toASCIIString().endsWith("file.owl"));
    }

    @Test
    public void usesCustomDelimiterWhenProvided() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " - ./file.owl";
        final File mappingFile = createMappingFile(content);
        final Map<String, String> properties = new HashMap<>();
        properties.put(MAPPING_FILE_LOCATION, mappingFile.getPath());
        properties.put(IRI_MAPPING_DELIMITER, "-");
        final Map<URI, URI> mappings = new MappingFileParser(properties).getMappings();
        assertEquals(1, mappings.size());
        assertTrue(mappings.get(URI.create(ontUri)).toASCIIString().endsWith("file.owl"));
    }

    @Test
    public void mappingFileSupportsRelativePaths() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " " + OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER + " file.owl";
        final File mappingFile = createMappingFile(content);
        final Map<URI, URI> mappings = new MappingFileParser(
                Collections.singletonMap(MAPPING_FILE_LOCATION, mappingFile.getPath())).getMappings();
        assertEquals(1, mappings.size());
        final URI targetPath = new File(mappingFile.getParent() + "/file.owl").toURI();
        assertEquals(targetPath, mappings.get(URI.create(ontUri)));
    }

    @Test
    public void mappingFileSupportsAbsolutePaths() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String targetPath = "/tmp/jopa/file.owl";
        final String content =
                ontUri + " " + OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER + " " + targetPath;
        final File mappingFile = createMappingFile(content);
        final Map<URI, URI> mappings = new MappingFileParser(
                Collections.singletonMap(MAPPING_FILE_LOCATION, mappingFile.getPath())).getMappings();
        assertEquals(1, mappings.size());
        assertEquals(new File(targetPath).toURI(), mappings.get(URI.create(ontUri)));
    }

    @Test
    public void mappingFileSupportsMultipleMappings() throws Exception {
        final List<URI> uris = generateUris();
        final List<String> files = generateFiles(uris.size());
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < uris.size(); i++) {
            sb.append(uris.get(i).toString());
            sb.append(" " + OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER + " ");
            sb.append(files.get(i));
            sb.append('\n');
        }
        final File mappingFile = createMappingFile(sb.toString());

        final Map<URI, URI> mappings = new MappingFileParser(
                Collections.singletonMap(MAPPING_FILE_LOCATION, mappingFile.getPath())).getMappings();
        assertEquals(uris.size(), mappings.size());
        for (int i = 0; i < uris.size(); i++) {
            final URI file = mappings.get(uris.get(i));
            final String filePath = files.get(i);
            if (filePath.startsWith("/")) {
                assertEquals(new File(filePath).toURI(), file);
            } else {
                assertEquals(new File(mappingFile.getParent() + "/" + filePath).toURI(), file);
            }
        }
    }

    @Test
    public void mappingFileParserSupportsRemoteUrls() throws Exception {
        final URI ontoUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa");
        final URI mappedTo = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa");
        final File mappingFile = createMappingFile(ontoUri.toString() + " > " + mappedTo.toString());

        final Map<URI, URI> mappings = new MappingFileParser(
                Collections.singletonMap(MAPPING_FILE_LOCATION, mappingFile.getPath())).getMappings();
        assertEquals(1, mappings.size());
        assertEquals(mappedTo, mappings.get(ontoUri));
    }

    @Test(expected = MappingFileParserException.class)
    public void invalidRemoteUrlMappedByMappingFileCausesParserException() throws Exception {
        final URI ontoUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa");
        final String mappedTo = "http:// .krizik.felk.cvut.cz";
        final File mappingFile = createMappingFile(ontoUri.toString() + " > " + mappedTo);

        new MappingFileParser(
                Collections.singletonMap(MAPPING_FILE_LOCATION, mappingFile.getPath())).getMappings();
    }

    @Test
    public void skipsLinesWithInvalidNumberOfTokens() throws Exception {
        final URI ontoUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa");
        final URI mappedTo = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa");
        final File mappingFile = createMappingFile(
                ontoUri.toString() + " > " + mappedTo.toString() + " > " + mappedTo.toString());

        final Map<URI, URI> mappings = new MappingFileParser(
                Collections.singletonMap(MAPPING_FILE_LOCATION, mappingFile.getPath())).getMappings();
        assertTrue(mappings.isEmpty());
    }

    private List<URI> generateUris() {
        final List<URI> lst = new ArrayList<>();
        final int cnt = TestUtils.randomInt(10);
        for (int i = 0; i < cnt; i++) {
            lst.add(URI.create("http://krizik.felk.cvut.cz/ontologies/test-" + i));
        }
        return lst;
    }

    private List<String> generateFiles(int count) {
        boolean absolute = false;
        final List<String> files = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            if (absolute) {
                files.add("/tmp/jopa/file_" + i + ".owl");
            } else {
                files.add("file_" + i + ".owl");
            }
            absolute = !absolute;
        }
        return files;
    }

    private static File createMappingFile(String content) throws Exception {
        final File tempFile = File.createTempFile("mf-parser-test", ".tmp");
        tempFile.deleteOnExit();
        try (final BufferedWriter bw = new BufferedWriter(new FileWriter(tempFile))) {
            bw.write(content);
        }
        return tempFile;
    }
}