/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.Mockito.when;

class FieldDescriptorTest {

    private static final URI CONTEXT_ONE = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
    private static final URI CONTEXT_TWO = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextTwo");

    static Stream<Arguments> dataGenerator() {
        return Stream.of(Arguments.arguments(null, null, "en", "cs", false),
                Arguments.arguments(CONTEXT_ONE, CONTEXT_TWO, "en", "en", false),
                Arguments.arguments(null, null, "en", "en", true),
                Arguments.arguments(CONTEXT_ONE, CONTEXT_ONE, "en", "en", true));
    }

    @Mock
    private FieldSpecification<TestClass, String> stringAtt;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
    }

    @ParameterizedTest
    @MethodSource("dataGenerator")
    void testEquality(URI contextOne, URI contextTwo, String langOne, String langTwo, boolean shouldBeEqual) {
        final Descriptor dOne = new FieldDescriptor(contextOne, stringAtt);
        final Descriptor dTwo = new FieldDescriptor(contextTwo, stringAtt);
        dOne.setLanguage(langOne);
        dTwo.setLanguage(langTwo);
        if (shouldBeEqual) {
            assertEquals(dOne, dTwo);
        } else {
            assertNotEquals(dOne, dTwo);
        }
    }
}
