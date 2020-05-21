package cz.cvut.kbss.jopa.util;

import cz.cvut.kbss.jopa.owlapi.exception.MappingFileParserException;
import org.junit.jupiter.api.Test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import static cz.cvut.kbss.jopa.util.MappingFileParser.DEFAULT_DELIMITER;
import static org.junit.jupiter.api.Assertions.*;

class MappingFileParserTest {

    @Test
    void throwsExceptionWhenMappingFileCannotBeFound() {
        final String someFilePath = "thisFile/does/not/exist";
        assertThrows(MappingFileParserException.class, () -> MappingFileParser.getMappings(new File(someFilePath)));
    }

    @Test
    public void getMappingsSupportsDefaultDelimiter() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " " + DEFAULT_DELIMITER + " ./file.owl";
        final File mappingFile = createMappingFile(content);
        final Map<URI, URI> mappings = MappingFileParser.getMappings(mappingFile);
        assertEquals(1, mappings.size());
        assertTrue(mappings.get(URI.create(ontUri)).toASCIIString().endsWith("file.owl"));
    }

    @Test
    public void getMappingsUsesCustomDelimiterWhenProvided() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " - ./file.owl";
        final File mappingFile = createMappingFile(content);
        final Map<URI, URI> mappings = MappingFileParser.getMappings(mappingFile, "-");
        assertNotNull(mappings);
        assertEquals(1, mappings.size());
        assertTrue(mappings.get(URI.create(ontUri)).toASCIIString().endsWith("file.owl"));
    }

    @Test
    public void mappingFileSupportsRelativePaths() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " " + DEFAULT_DELIMITER + " file.owl";
        final File mappingFile = createMappingFile(content);
        final Map<URI, URI> mappings = MappingFileParser.getMappings(mappingFile);
        assertEquals(1, mappings.size());
        final URI targetPath = new File(mappingFile.getParent() + "/file.owl").toURI();
        assertEquals(targetPath, mappings.get(URI.create(ontUri)));
    }

    @Test
    public void mappingFileSupportsAbsolutePaths() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String targetPath = "/tmp/jopa/file.owl";
        final String content = ontUri + " " + DEFAULT_DELIMITER + " " + targetPath;
        final File mappingFile = createMappingFile(content);
        final Map<URI, URI> mappings = MappingFileParser.getMappings(mappingFile);
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
            sb.append(" " + DEFAULT_DELIMITER + " ");
            sb.append(files.get(i));
            sb.append('\n');
        }
        final File mappingFile = createMappingFile(sb.toString());

        final Map<URI, URI> mappings = MappingFileParser.getMappings(mappingFile);
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

        final Map<URI, URI> mappings = MappingFileParser.getMappings(mappingFile);
        assertEquals(1, mappings.size());
        assertEquals(mappedTo, mappings.get(ontoUri));
    }

    @Test
    public void invalidRemoteUrlMappedByMappingFileCausesParserException() throws Exception {
        final URI ontoUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa");
        final String mappedTo = "http:// .krizik.felk.cvut.cz";
        final File mappingFile = createMappingFile(ontoUri.toString() + " > " + mappedTo);

        assertThrows(MappingFileParserException.class, () -> MappingFileParser.getMappings(mappingFile));
    }

    @Test
    public void skipsLinesWithInvalidNumberOfTokens() throws Exception {
        final URI ontoUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa");
        final URI mappedTo = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa");
        final File mappingFile = createMappingFile(
                ontoUri.toString() + " " + DEFAULT_DELIMITER + " " + mappedTo.toString() + " " + DEFAULT_DELIMITER +
                        " " + mappedTo.toString());

        final Map<URI, URI> mappings = MappingFileParser.getMappings(mappingFile);
        assertTrue(mappings.isEmpty());
    }

    private static List<URI> generateUris() {
        final Random random = new Random();
        final List<URI> lst = new ArrayList<>();
        final int cnt = random.nextInt(10);
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
