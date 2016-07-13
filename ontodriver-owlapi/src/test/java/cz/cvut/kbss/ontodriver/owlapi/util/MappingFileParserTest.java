/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiDataSource;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiConfigParam;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiOntoDriverProperties;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.exception.MappingFileParserException;
import org.junit.Test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class MappingFileParserTest {

    private Configuration configuration = new Configuration(
            OntologyStorageProperties.driver(OwlapiDataSource.class.getName()).physicalUri("testFile").build());

    @Test(expected = MappingFileParserException.class)
    public void throwsExceptionWhenMappingFileCannotBeFound() throws Exception {
        final String someFilePath = "thisFile/does/not/exist";
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, someFilePath);
        new MappingFileParser(configuration);
    }

    @Test
    public void mappingFileParserAlsoAcceptsURIAsMappingFilePath() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " " +
                OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER + " ./file.owl";
        final File mappingFile = createMappingFile(content);
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.toURI().toString());
        final Map<URI, URI> mappings = new MappingFileParser(configuration).getMappings();
        assertEquals(1, mappings.size());
    }

    @Test
    public void usesDefaultDelimiterWhenNoneIsProvided() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " " +
                OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER + " ./file.owl";
        final File mappingFile = createMappingFile(content);
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.getPath());
        final Map<URI, URI> mappings = new MappingFileParser(configuration).getMappings();
        assertEquals(1, mappings.size());
        assertTrue(mappings.get(URI.create(ontUri)).toASCIIString().endsWith("file.owl"));
    }

    @Test
    public void usesCustomDelimiterWhenProvided() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " - ./file.owl";
        final File mappingFile = createMappingFile(content);
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.getPath());
        configuration.setProperty(OwlapiConfigParam.IRI_MAPPING_DELIMITER, "-");
        final Map<URI, URI> mappings = new MappingFileParser(configuration).getMappings();
        assertEquals(1, mappings.size());
        assertTrue(mappings.get(URI.create(ontUri)).toASCIIString().endsWith("file.owl"));
    }

    @Test
    public void mappingFileSupportsRelativePaths() throws Exception {
        final String ontUri = "http://krizik.felk.cvut.cz/ontologies/test";
        final String content = ontUri + " " + OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER + " file.owl";
        final File mappingFile = createMappingFile(content);
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.getPath());
        final Map<URI, URI> mappings = new MappingFileParser(configuration).getMappings();
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
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.getPath());
        final Map<URI, URI> mappings = new MappingFileParser(configuration).getMappings();
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
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.getPath());

        final Map<URI, URI> mappings = new MappingFileParser(configuration).getMappings();
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
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.getPath());

        final Map<URI, URI> mappings = new MappingFileParser(configuration).getMappings();
        assertEquals(1, mappings.size());
        assertEquals(mappedTo, mappings.get(ontoUri));
    }

    @Test(expected = MappingFileParserException.class)
    public void invalidRemoteUrlMappedByMappingFileCausesParserException() throws Exception {
        final URI ontoUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa");
        final String mappedTo = "http:// .krizik.felk.cvut.cz";
        final File mappingFile = createMappingFile(ontoUri.toString() + " > " + mappedTo);
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.getPath());

        new MappingFileParser(configuration).getMappings();
    }

    @Test
    public void skipsLinesWithInvalidNumberOfTokens() throws Exception {
        final URI ontoUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa");
        final URI mappedTo = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa");
        final File mappingFile = createMappingFile(
                ontoUri.toString() + " > " + mappedTo.toString() + " > " + mappedTo.toString());
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, mappingFile.getPath());

        final Map<URI, URI> mappings = new MappingFileParser(configuration).getMappings();
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