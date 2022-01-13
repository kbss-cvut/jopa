/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.utils;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URL;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

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
}
