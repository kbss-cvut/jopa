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
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.prefix.PrefixMap;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyID;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
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
        assertEquals("navrzeny_pojem", sut.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriSanitizesJavaKeywords() {
        final IRI iri = IRI.create(PREFIX + "volatile");
        assertEquals("_volatile", sut.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriUsesFragmentWhenIriContainsIt() {
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/owl2java-test#Fragment");
        assertEquals("Fragment", sut.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriUsesPartBetweenLastHashAndFragmentWhenFragmentIsLastChar() {
        final IRI iri = IRI.create("http://www.w3.org/ns/activitystreams#");
        assertEquals("activitystreams", sut.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriUsesPartBetweenLastAndSecondToLastSlashWhenSlashIsLastChar() {
        final IRI iri = IRI.create("http://purl.org/vocab/vann/");
        assertEquals("vann", sut.generateJavaNameForIri(iri));
    }

    @Test
    void generatePrefixedJavaNameForIriUsesPrefixRegisteredForOntologyIri() {
        final IRI ontologyIri = IRI.create(RDFS.NAMESPACE);
        when(prefixMap.getPrefix(ontologyIri)).thenReturn(Optional.of(RDFS.PREFIX));
        assertEquals("rdfs_label", sut.generatePrefixedJavaNameForIri(IRI.create(RDFS.LABEL), new OWLOntologyID(ontologyIri)));
    }

    @Test
    void generatePrefixedJavaNameForIriResolvesPrefixFromOntologyIriWhenNoPrefixIsRegisteredInPrefixMap() {
        final IRI ontologyIri = IRI.create("http://www.w3.org/ns/activitystreams#");
        final IRI iri = IRI.create("https://www.w3.org/ns/activitystreams#Event");
        when(prefixMap.getPrefix(any())).thenReturn(Optional.empty());
        assertEquals("activitystreams_Event", sut.generatePrefixedJavaNameForIri(iri, new OWLOntologyID(ontologyIri)));
    }

    @Test
    void generatePrefixedJavaNameForIriReturnsNameWithoutPrefixWhenOntologyIdIsAnonymous() {
        final IRI iri = IRI.create("https://www.w3.org/ns/activitystreams#Event");
        assertEquals("Event", sut.generatePrefixedJavaNameForIri(iri, new OWLOntologyID()));
        verify(prefixMap, never()).getPrefix(any());
    }

    @Test
    void generatedPrefixedJavaNameForIriReturnsValidJavaNameWhenPrefixContainsDashes() {
        final IRI ontologyIri = IRI.create("http://onto.fel.cvut.cz/ontologies/slovn\\u00edk/agendov\\u00fd/popis-dat");
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/slovn\\u00edk/agendov\\u00fd/popis-dat/pojem/atribut");
        when(prefixMap.getPrefix(ontologyIri)).thenReturn(Optional.of("popis-dat"));
        assertEquals("popis_dat_atribut", sut.generatePrefixedJavaNameForIri(iri, new OWLOntologyID(ontologyIri)));
    }
}
