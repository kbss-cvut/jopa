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
package cz.cvut.kbss.ontodriver.model;

import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;

class AssertionTest {

    private static final URI ID = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/ontodriver/test");

    @Test
    void hasLanguageReturnsFalseWhenLanguageIsExplicitlySetToNull() {
        final Assertion sut = Assertion.createDataPropertyAssertion(ID, null, false);
        assertFalse(sut.hasLanguage());
    }

    @Test
    void equalsReturnsFalseWhenAssertionsDifferInLanguage() {
        final Assertion aOne = Assertion.createDataPropertyAssertion(ID, "cs", false);
        final Assertion aTwo = Assertion.createDataPropertyAssertion(ID, "en", false);
        assertNotEquals(aOne, aTwo);
    }

    @Test
    void equalsReturnsFalseWhenAssertionsDifferInInferenceType() {
        final Assertion aOne = Assertion.createDataPropertyAssertion(ID, false);
        final Assertion aTwo = Assertion.createDataPropertyAssertion(ID, true);
        assertNotEquals(aOne, aTwo);
    }
}