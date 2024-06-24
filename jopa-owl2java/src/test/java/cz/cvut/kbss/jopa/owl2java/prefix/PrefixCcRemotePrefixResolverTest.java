/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PrefixCcRemotePrefixResolverTest {

    private final PrefixCcRemotePrefixResolver sut = new PrefixCcRemotePrefixResolver();

    @Test
    void resolvePrefixReturnsPrefixReturnedByPrefixCcService() {
        final Optional<String> result = sut.resolvePrefix(IRI.create("http://xmlns.com/foaf/0.1/"));
        assertTrue(result.isPresent());
        assertEquals("foaf", result.get());
    }

    @Test
    void resolvePrefixReturnsEmptyOptionalWhenPrefixCcServiceReturnsNotFound() {
        final Optional<String> result = sut.resolvePrefix(IRI.create(Generator.generateUri()));
        assertTrue(result.isEmpty());
    }
}
