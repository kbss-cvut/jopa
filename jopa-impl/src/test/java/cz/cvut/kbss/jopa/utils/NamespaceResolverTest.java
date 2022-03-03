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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class NamespaceResolverTest {

    private final NamespaceResolver sut = new NamespaceResolver();

    @Test
    void resolveReturnsOriginalIriWhenItDoesNotContainPrefix() {
        assertEquals(DC.Elements.DESCRIPTION, sut.resolveFullIri(DC.Elements.DESCRIPTION));
    }

    @Test
    void resolveReturnsOriginalIriWhenPrefixIsNotRegistered() {
        final String iri = "dc:description";
        assertEquals(iri, sut.resolveFullIri(iri));
    }

    @Test
    void resolveReturnsPreregisteredIri() {
        assertEquals(RDFS.LABEL, sut.resolveFullIri("rdfs:label"));
    }

    @Test
    void resolveReturnsIriBasedOnRegisteredNamespace() {
        sut.registerNamespace("dc", DC.Elements.NAMESPACE);
        final String iri = "dc:description";
        assertEquals(DC.Elements.NAMESPACE + "description", sut.resolveFullIri(iri));
    }

    @Test
    void resolveReturnsOriginalIriWhenPrefixIsInvalid() {
        final String invalidIri = "prefixWithoutLocalName";
        assertEquals(invalidIri, sut.resolveFullIri(invalidIri));
    }

    @Test
    void resolveReturnsOriginalValueWhenItIsURN() {
        final String urn = "urn:jopa:model:test-class";
        assertEquals(urn, sut.resolveFullIri(urn));
    }
}
