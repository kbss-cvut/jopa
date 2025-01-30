/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
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

    @Test
    void getAttributeDescriptorReturnsThisInstanceWhenAttributeMatches() {
        final Descriptor d = new FieldDescriptor(stringAtt);
        assertSame(d, d.getAttributeDescriptor(stringAtt));
    }

    @Test
    void getAttributeDescriptorThrowsIllegalArgumentWhenRequestedAttributeIsNotRepresentedByThisDescriptor() {
        final Descriptor d = new FieldDescriptor(stringAtt);
        final FieldSpecification<TestClass, Integer> intAtt = mock(FieldSpecification.class);
        assertThrows(IllegalArgumentException.class, () -> d.getAttributeDescriptor(intAtt));
    }
}
