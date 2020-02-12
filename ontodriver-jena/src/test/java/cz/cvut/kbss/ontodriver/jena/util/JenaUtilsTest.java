/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.Test;

import static org.junit.Assert.*;

public class JenaUtilsTest {

    @Test
    public void isResourceReturnsTrueForUri() {
        assertTrue(JenaUtils.isResourceIdentifier(Generator.generateUri()));
    }

    @Test
    public void isResourceReturnsTrueForNamedResource() {
        assertTrue(JenaUtils.isResourceIdentifier(NamedResource.create(Generator.generateUri())));
    }

    @Test
    public void isResourceReturnsTrueForStringUri() {
        assertTrue(JenaUtils.isResourceIdentifier(Generator.generateUri().toString()));
    }

    @Test
    public void isResourceReturnsFalseForNonStringValue() {
        assertFalse(JenaUtils.isResourceIdentifier(117));
    }

    @Test
    public void isResourceReturnsFalseForNonUriStringValue() {
        assertFalse(JenaUtils.isResourceIdentifier("jdf123"));
    }

    @Test
    public void literalToValueReturnsTypeRepresentedByLiteral() {
        assertEquals(117, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(117)));
        assertEquals(true, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(true)));
        assertEquals("test", JenaUtils.literalToValue(ResourceFactory.createTypedLiteral("test")));
    }

    @Test
    public void literalToValueTranslatesLongLiteralToJavaLong() {
        assertEquals(117L, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(117L)));
    }
}