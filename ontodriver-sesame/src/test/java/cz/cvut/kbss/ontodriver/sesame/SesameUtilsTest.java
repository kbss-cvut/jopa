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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.XMLSchema;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Optional;

import static org.junit.Assert.*;

public class SesameUtilsTest {

    private static final String LANG = "en";

    private static ValueFactory vf;

    private static MemoryStore memoryStore;

    private enum Severity {
        LOW, MEDIUM
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        memoryStore = new MemoryStore();
        memoryStore.initialize();
        vf = memoryStore.getValueFactory();
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        memoryStore.shutDown();
    }

    @Test
    public void enumLiteralIsReturnedAsStringValue() throws Exception {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Optional<Object> result = SesameUtils.getDataPropertyValue(literal, LANG);
        assertEquals(Severity.LOW.toString(), result.get());
    }

    @Test
    public void stringLiteralWithLanguageTagNotMatchingExpectedReturnsEmptyOptional() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Optional<Object> result = SesameUtils.getDataPropertyValue(literal, "cs");
        assertFalse(result.isPresent());
    }

    @Test
    public void stringLiteralIsReturnedWhenItsLanguageMatches() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Optional<Object> result = SesameUtils.getDataPropertyValue(literal, LANG);
        assertTrue(result.isPresent());
        assertEquals(Severity.LOW.toString(), result.get());
    }

    @Test
    public void stringLiteralIsReturnedWhenNoLanguageIsSpecified() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Optional<Object> result = SesameUtils.getDataPropertyValue(literal, null);
        assertTrue(result.isPresent());
        assertEquals(Severity.LOW.toString(), result.get());
    }

    @Test
    public void enumValueIsReturnedAsStringLiteral() throws Exception {
        final Literal literal = SesameUtils.createDataPropertyLiteral(Severity.MEDIUM, LANG, vf);
        assertNotNull(literal);
        assertEquals(Severity.MEDIUM.toString(), literal.stringValue());
        assertTrue(literal.getDatatype() == null || literal.getDatatype().equals(XMLSchema.STRING));
    }
}