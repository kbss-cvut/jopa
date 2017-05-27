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
package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiConfigParam;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiOntoDriverProperties;
import cz.cvut.kbss.ontodriver.owlapi.exception.MappingFileParserException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

public class MappingFileParser {

    private static final Logger LOG = LoggerFactory.getLogger(MappingFileParser.class);

    private static final String[] REMOTE_URL_SCHEMES = {"http://", "https://", "ftp://", "sftp://"};

    private final File mappingFile;

    private final String delimiter;

    public MappingFileParser(Configuration configuration) {
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
        final Map<URI, URI> map = new HashMap<>();
        String line;
        final File defaultDir = mappingFile.getParentFile();
        try (final BufferedReader r = new BufferedReader(new FileReader(mappingFile))) {
            while ((line = r.readLine()) != null) {
                final StringTokenizer t = new StringTokenizer(line, delimiter);
                if (t.countTokens() != 2) {
                    LOG.warn("Ignoring line '" + line + "' - invalid number of tokens = " + t.countTokens());
                    continue;
                }
                final String uriName = t.nextToken().trim();
                final String fileName = t.nextToken().trim();
                final URI fileUri = resolveLocation(defaultDir, fileName);

                LOG.trace("Mapping ontology {} to location {}.", uriName, fileUri);
                map.put(URI.create(uriName), fileUri);
            }
        } catch (IOException e) {
            LOG.error("Unable to parse mapping file." + e);
            throw new MappingFileParserException(e);
        }
        return map;
    }

    private URI resolveLocation(File defaultDir, String targetUri) {
        for (String scheme : REMOTE_URL_SCHEMES) {
            if (targetUri.startsWith(scheme)) {
                try {
                    return URI.create(targetUri);
                } catch (IllegalArgumentException e) {
                    LOG.error("Target URI {} looks like a remote URI, but is not valid.", targetUri);
                    throw new MappingFileParserException(e);
                }
            }
        }
        final File actualFile =
                new File(targetUri).isAbsolute() ? new File(targetUri) : new File(defaultDir, targetUri);
        return actualFile.toURI();
    }
}
