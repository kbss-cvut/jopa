/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.util;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.StringTokenizer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cz.cvut.kbss.jopa.owlapi.exception.MappingFileParserException;

public final class MappingFileParser {

    private static final Logger LOG = LoggerFactory.getLogger(MappingFileParser.class);

    /**
     * Default delimiter of mappings in the mapping file.
     * <p>
     * That is, on the left-hand side of the delimiter would the original IRI and on the right-hand side is the mapped
     * value.
     */
    public static final String DEFAULT_DELIMITER = ">";

    private static final String[] REMOTE_URL_SCHEMES = {"http://", "https://", "ftp://", "sftp://"};

    private MappingFileParser() {
        throw new AssertionError();
    }

    /**
     * Retrieves IRI mappings from the specified file using the default mapping delimiter - {@link #DEFAULT_DELIMITER}.
     *
     * @param mappingFile Mapping file to use
     * @return Map of IRI mappings
     */
    public static Map<URI, URI> getMappings(final File mappingFile) {
        return getMappings(mappingFile, DEFAULT_DELIMITER);
    }

    /**
     * Retrieves IRI mappings from the specified file using the specified mapping delimiter.
     *
     * @param mappingFile Mapping file to use
     * @param delimiter   Delimiter of the mapped IRI
     * @return Map of IRI mappings
     */
    public static Map<URI, URI> getMappings(final File mappingFile, String delimiter) {
        Objects.requireNonNull(mappingFile);
        Objects.requireNonNull(delimiter);
        final Map<URI, URI> map = new HashMap<>();
        final File defaultDir = mappingFile.getParentFile();
        try {
            final List<String> lines = Files.readAllLines(mappingFile.toPath(), StandardCharsets.UTF_8);
            lines.forEach(line -> {
            final StringTokenizer t = new StringTokenizer(line, delimiter);
            if (t.countTokens() != 2) {
                LOG.warn("Ignoring line '{}' - invalid number of tokens = {}", line, t.countTokens());
                return;
            }
            final String uriName = t.nextToken().trim();
            final String fileName = t.nextToken().trim();
            final URI fileUri = resolveLocation(defaultDir, fileName);

            LOG.debug("Mapping ontology {} to location {}.", uriName, fileUri);
            map.put(URI.create(uriName), fileUri);
        });
        } catch (IOException e) {
            LOG.error("Unable to parse mapping file.", e);
            throw new MappingFileParserException(e);
        }
        return map;
    }

    private static URI resolveLocation(File defaultDir, String targetUri) {
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
