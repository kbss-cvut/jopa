/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.openrdf.model.Literal;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.XMLSchema;
import org.openrdf.sail.memory.MemoryStore;

import static org.junit.Assert.*;

/**
 * @author ledvima1
 */
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
        final Object result = SesameUtils.getDataPropertyValue(literal);
        assertEquals(Severity.LOW.toString(), result);
    }

    @Test
    public void enumValueIsReturnedAsStringLiteral() throws Exception {
        final Literal literal = SesameUtils.createDataPropertyLiteral(Severity.MEDIUM, LANG, vf);
        assertNotNull(literal);
        assertEquals(Severity.MEDIUM.toString(), literal.stringValue());
        assertTrue(literal.getDatatype() == null || literal.getDatatype().equals(XMLSchema.STRING));
    }
}