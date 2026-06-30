/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.prefix.PrefixMap;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.semanticweb.owlapi.model.IRI;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class JavaNameGeneratorTest {

    private static final String PREFIX = "http://onto.fel.cvut.cz/ontologies/owl2java-test/";

    @Mock
    private PrefixMap prefixMap;

    @InjectMocks
    private JavaNameGenerator sut;

    @Test
    void toCamelCaseNotationCamelCasesStringsSeparatedByUnderscore() {
        assertEquals("TestValueCamelCased", JavaNameGenerator.toCamelCaseNotation("test_value_camel_cased"));
    }

    @Test
    void generateJavaNameForIriGeneratesValidJavaIdentifiersForIriWithNonAsciiCharacters() {
        final IRI iri = IRI.create(PREFIX + "navržený-pojem");
        assertEquals("navrzeny_pojem", JavaNameGenerator.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriSanitizesJavaKeywords() {
        final IRI iri = IRI.create(PREFIX + "volatile");
        assertEquals("_volatile", JavaNameGenerator.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriUsesFragmentWhenIriContainsIt() {
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/owl2java-test#Fragment");
        assertEquals("Fragment", JavaNameGenerator.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriUsesPartBetweenLastHashAndFragmentWhenFragmentIsLastChar() {
        final IRI iri = IRI.create("http://www.w3.org/ns/activitystreams#");
        assertEquals("activitystreams", JavaNameGenerator.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriUsesPartBetweenLastAndSecondToLastSlashWhenSlashIsLastChar() {
        final IRI iri = IRI.create("http://purl.org/vocab/vann/");
        assertEquals("vann", JavaNameGenerator.generateJavaNameForIri(iri));
    }

    @Test
    void generatePrefixedJavaNameForIriUsesPrefixRegisteredForNamespace() {
        final IRI iri = IRI.create(RDFS.LABEL);
        when(prefixMap.getPrefix(iri)).thenReturn(Optional.of(RDFS.PREFIX));
        assertEquals("rdfs_label", sut.generatePrefixedJavaNameForIri(iri));
    }

    @Test
    void generatePrefixedJavaNameForIriReturnsNameWithoutPrefixWhenNoPrefixIsRegistered() {
        final IRI iri = IRI.create("https://www.w3.org/ns/activitystreams#Event");
        when(prefixMap.getPrefix(any())).thenReturn(Optional.empty());
        assertEquals("Event", sut.generatePrefixedJavaNameForIri(iri));
    }

    @Test
    void generatedPrefixedJavaNameForIriReturnsValidJavaNameWhenPrefixContainsDashes() {
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/slovn\\u00edk/agendov\\u00fd/popis-dat/pojem/atribut");
        when(prefixMap.getPrefix(iri)).thenReturn(Optional.of("popis-dat"));
        assertEquals("popis_dat_atribut", sut.generatePrefixedJavaNameForIri(iri));
    }
}
