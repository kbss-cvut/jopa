/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.descriptors;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.net.URI;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

class FieldDescriptorTest {

    private static final URI CONTEXT_ONE = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
    private static final URI CONTEXT_TWO = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextTwo");

    static Stream<Arguments> dataGenerator() {
        return Stream.of(Arguments.arguments(null, null, "en", "cs", false),
                Arguments.arguments(CONTEXT_ONE, CONTEXT_TWO, "en", "en", false),
                Arguments.arguments(null, null, "en", "en", true),
                Arguments.arguments(CONTEXT_ONE, CONTEXT_ONE, "en", "en", true));
    }

    @ParameterizedTest
    @MethodSource("dataGenerator")
    void testEquality(URI contextOne, URI contextTwo, String langOne, String langTwo, boolean shouldBeEqual)
            throws Exception {
        final Descriptor dOne = new FieldDescriptor(contextOne, TestClass.stringAttField());
        final Descriptor dTwo = new FieldDescriptor(contextTwo, TestClass.stringAttField());
        dOne.setLanguage(langOne);
        dTwo.setLanguage(langTwo);
        if (shouldBeEqual) {
            assertEquals(dOne, dTwo);
        } else {
            assertNotEquals(dOne, dTwo);
        }
    }
}