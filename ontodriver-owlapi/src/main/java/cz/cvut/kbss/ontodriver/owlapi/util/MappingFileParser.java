/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.jopa.owlapi.exception.MappingFileParserException;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiConfigParam;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiOntoDriverProperties;

import java.io.File;
import java.net.URI;
import java.util.Map;

public class MappingFileParser {

    private final File mappingFile;

    private final String delimiter;

    public MappingFileParser(DriverConfiguration configuration) {
        final String mappingFilePath = configuration.getProperty(OwlapiConfigParam.MAPPING_FILE_LOCATION);
        assert mappingFilePath != null;
        this.mappingFile = resolveMappingFile(mappingFilePath);
        this.delimiter = configuration.getProperty(OwlapiConfigParam.IRI_MAPPING_DELIMITER,
                OwlapiOntoDriverProperties.DEFAULT_IRI_MAPPING_DELIMITER);
    }

    private File resolveMappingFile(String path) {
        File mapping = new File(path);
        if (mapping.exists()) {
            return mapping;
        }
        try {
            mapping = new File(URI.create(path));
            if (mapping.exists()) {
                return mapping;
            }
        } catch (IllegalArgumentException e) {
            throw new MappingFileParserException(
                    "Mapping file path " + path + " is neither a valid file path nor a valid URI.", e);
        }
        throw new MappingFileParserException("Mapping file " + path + " does not exist.");
    }

    public Map<URI, URI> getMappings() {
        assert mappingFile != null;

        return cz.cvut.kbss.jopa.util.MappingFileParser.getMappings(mappingFile, delimiter);
    }
}
