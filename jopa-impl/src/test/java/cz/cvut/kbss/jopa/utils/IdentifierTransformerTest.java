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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URL;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class IdentifierTransformerTest {

    private static final String STR_IDENTIFIER = "http://krizik.felk.cvut.cz/ontologies/jopa";
    private static final URI IDENTIFIER = URI.create(STR_IDENTIFIER);

    @Test
    void transformUriToUri() {
        final Object res = IdentifierTransformer.transformToIdentifier(IDENTIFIER, URI.class);
        assertTrue(res instanceof URI);
        assertEquals(STR_IDENTIFIER, res.toString());
    }

    @Test
    void transformUriToString() {
        final Object res = IdentifierTransformer.transformToIdentifier(IDENTIFIER, String.class);
        assertTrue(res instanceof String);
        assertEquals(STR_IDENTIFIER, res);
    }

    @Test
    void transformUriToUrl() {
        final Object res = IdentifierTransformer.transformToIdentifier(IDENTIFIER, URL.class);
        assertTrue(res instanceof URL);
        assertEquals(STR_IDENTIFIER, res.toString());
    }

    @Test
    void transformThrowsIllegalArgumentForUnsupportedTargetType() {
        assertThrows(IllegalArgumentException.class,
                () -> IdentifierTransformer.transformToIdentifier(IDENTIFIER, Date.class));
    }

    @Test
    void stringifyIriReturnsSpecifiedIriInAngledBrackets() {
        final URI uri = Generators.createIndividualIdentifier();
        assertEquals("<" + uri + ">", IdentifierTransformer.stringifyIri(uri));
    }
}
