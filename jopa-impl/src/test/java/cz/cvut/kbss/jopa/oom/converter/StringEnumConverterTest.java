/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.datatype.exception.UnsupportedTypeTransformationException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class StringEnumConverterTest {

    @Test
    public void convertToAxiomValueTransformsEnumValueToString() {
        final StringEnumConverter<OWLClassM.Severity> sut = new StringEnumConverter<>(OWLClassM.Severity.class);
        final Object result = sut.convertToAxiomValue(OWLClassM.Severity.HIGH);
        assertInstanceOf(String.class, result);
        assertEquals(OWLClassM.Severity.HIGH.toString(), result);
    }

    @Test
    public void convertToAttributeValueUsesEnumValueOf() {
        final StringEnumConverter<OWLClassM.Severity> sut = new StringEnumConverter<>(OWLClassM.Severity.class);
        final OWLClassM.Severity result = sut.convertToAttribute(OWLClassM.Severity.HIGH.toString());
        assertEquals(OWLClassM.Severity.HIGH, result);
    }

    @Test
    public void convertToAttributeValueThrowsUnsupportedTypeTransformationForUnknownEnumValue() {
        final StringEnumConverter<OWLClassM.Severity> sut = new StringEnumConverter<>(OWLClassM.Severity.class);
        assertThrows(UnsupportedTypeTransformationException.class, () -> sut.convertToAttribute("test"));
    }

    @Test
    void convertToAxiomValueHandlesNullValue() {
        final StringEnumConverter<OWLClassM.Severity> sut = new StringEnumConverter<>(OWLClassM.Severity.class);
        assertNull(sut.convertToAxiomValue(null));
    }
}
