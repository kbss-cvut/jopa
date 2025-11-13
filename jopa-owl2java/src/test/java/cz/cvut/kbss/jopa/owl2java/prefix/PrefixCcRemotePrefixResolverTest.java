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
package cz.cvut.kbss.jopa.owl2java.prefix;

import cz.cvut.kbss.jopa.owl2java.environment.Generator;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.IRI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PrefixCcRemotePrefixResolverTest {

    private static final Logger LOG = LoggerFactory.getLogger(PrefixCcRemotePrefixResolverTest.class);

    private final PrefixCcRemotePrefixResolver sut = new PrefixCcRemotePrefixResolver();

    @Test
    void resolvePrefixReturnsPrefixReturnedByPrefixCcService() {
        if (isPrefixCcServiceUnavailable()) {
            LOG.warn("Prefix.cc service is unavailable.");
            return;
        }
        final Optional<String> result = sut.resolvePrefix(IRI.create("http://xmlns.com/foaf/0.1/"));
        assertTrue(result.isPresent());
        assertEquals("foaf", result.get());
    }

    private static boolean isPrefixCcServiceUnavailable() {
        try {
            final HttpURLConnection urlConnection = (HttpURLConnection) new URL("https://prefix.cc").openConnection();
            return urlConnection.getResponseCode() != 200;
        } catch (IOException e) {
            return true;
        }
    }

    @Test
    void resolvePrefixReturnsEmptyOptionalWhenPrefixCcServiceReturnsNotFound() {
        if (isPrefixCcServiceUnavailable()) {
            LOG.warn("Prefix.cc service is unavailable.");
            return;
        }
        final Optional<String> result = sut.resolvePrefix(IRI.create(Generator.generateUri()));
        assertTrue(result.isEmpty());
    }
}
