/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.jopa.owlapi.exception.MappingFileParserException;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiDataSource;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiConfigParam;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiOntoDriverProperties;
import org.junit.jupiter.api.Test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.net.URI;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class MappingFileParserTest {

    private final DriverConfiguration configuration = new DriverConfiguration(
            OntologyStorageProperties.driver(OwlapiDataSource.class.getName()).physicalUri("testFile").build());

    @Test
    public void throwsExceptionWhenMappingFileCannotBeFound() {
        final String someFilePath = "thisFile/does/not/exist";
        configuration.setProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION, someFilePath);
        assertThrows(MappingFileParserException.class, () -> new MappingFileParser(configuration));
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

    private static File createMappingFile(String content) throws Exception {
        final File tempFile = File.createTempFile("mf-parser-test", ".tmp");
        tempFile.deleteOnExit();
        try (final BufferedWriter bw = new BufferedWriter(new FileWriter(tempFile))) {
            bw.write(content);
        }
        return tempFile;
    }
}
