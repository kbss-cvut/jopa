/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

import org.junit.Test;

import java.net.URI;
import java.net.URL;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author ledvima1
 */
public class IdentifierTransformerTest {

    private static final String STR_IDENTIFIER = "http://krizik.felk.cvut.cz/ontologies/jopa";
    private static final URI IDENTIFIER = URI.create(STR_IDENTIFIER);

    private IdentifierTransformer transformer = new IdentifierTransformer();

    @Test
    public void transformUriToUri() throws Exception {
        final Object res = transformer.transformToType(IDENTIFIER, URI.class);
        assertTrue(res instanceof URI);
        assertEquals(STR_IDENTIFIER, res.toString());
    }

    @Test
    public void transformUriToString() throws Exception {
        final Object res = transformer.transformToType(IDENTIFIER, String.class);
        assertTrue(res instanceof String);
        assertEquals(STR_IDENTIFIER, res);
    }

    @Test
    public void transformUriToUrl() throws Exception {
        final Object res = transformer.transformToType(IDENTIFIER, URL.class);
        assertTrue(res instanceof URL);
        assertEquals(STR_IDENTIFIER, res.toString());
    }
}