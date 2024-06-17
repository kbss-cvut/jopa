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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URL;
import java.util.Collections;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

class ObjectConverterTest {

    private ObjectConverter sut = new ObjectConverter(true);

    @Test
    void convertToAxiomValueDoesNothingToLiteralValues() {
        final Object valueOne = "117";
        final Object valueTwo = 1237;
        assertSame(valueOne, sut.convertToAxiomValue(valueOne));
        assertSame(valueTwo, sut.convertToAxiomValue(valueTwo));
    }

    @Test
    void convertToAxiomValueTransformsIdentifierValueToNamedResource() throws Exception {
        final URL valueOne = Generators.createIndividualIdentifier().toURL();
        final URI valueTwo = Generators.createIndividualIdentifier();
        assertEquals(NamedResource.create(valueOne.toString()), sut.convertToAxiomValue(valueOne));
        assertEquals(NamedResource.create(valueTwo), sut.convertToAxiomValue(valueTwo));
    }

    @Test
    void convertToAttributeDoesNothingToLiteralValues() {
        final Object valueOne = new Date();
        final Object valueTwo = 1237;
        assertSame(valueOne, sut.convertToAttribute(valueOne));
        assertSame(valueTwo, sut.convertToAttribute(valueTwo));
    }

    @Test
    void convertToAttributeTransformsNamedResourceToUri() {
        final NamedResource value = NamedResource.create(Generators.createIndividualIdentifier());
        assertEquals(value.getIdentifier(), sut.convertToAttribute(value));
    }

    @Test
    void convertToAttributeTransformsLangStringToMultilingualStringWhenMultilingualStringsArePreferred() {
        final LangString value = new LangString("test", "en");
        assertEquals(new MultilingualString(Collections.singletonMap("en", "test")), sut.convertToAttribute(value));
    }

    @Test
    void convertToAttributeTransformsLangStringToStringWhenMultilingualStringsAreNotPreferred() {
        this.sut = new ObjectConverter(false);
        final LangString value = new LangString("test", "en");
        assertEquals(value.getValue(), sut.convertToAttribute(value));
    }
}
