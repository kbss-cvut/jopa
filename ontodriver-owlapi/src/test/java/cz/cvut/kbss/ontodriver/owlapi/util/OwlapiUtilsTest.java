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
package cz.cvut.kbss.ontodriver.owlapi.util;

import org.junit.Test;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import static org.junit.Assert.*;

public class OwlapiUtilsTest {

    private static final String LANG = "en";

    private OWLDataFactory dataFactory = new OWLDataFactoryImpl();

    @Test
    public void doesLanguageMatchReturnsTrueWhenLanguageTagMatches() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, LANG));
    }

    @Test
    public void doesLanguageMatchReturnsFalseWhenLanguageTagDoesNotMatch() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        assertFalse(OwlapiUtils.doesLanguageMatch(literal, "cs"));
    }

    @Test
    public void doesLanguageMatchReturnsTrueWhenLanguageIsNotSpecified() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, null));
    }

    @Test
    public void doesLanguageMatchReturnsTrueWhenLiteralHasNoLanguageTag() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test");
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, LANG));
    }

    @Test
    public void doesLanguageMatchReturnsTrueForNonString() {
        final OWLLiteral literal = dataFactory.getOWLLiteral(117);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, LANG));
    }
}