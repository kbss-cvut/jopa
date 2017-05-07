package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.CommonVocabulary;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class NamespaceResolverTest {

    private final NamespaceResolver resolver = new NamespaceResolver();

    @Test
    public void resolveReturnsOriginalIriWhenItDoesNotContainPrefix() {
        final String iri = "http://purl.org/dc/elements/1.1/description";
        assertEquals(iri, resolver.resolveFullIri(iri));
    }

    @Test
    public void resolveReturnsOriginalIriWhenPrefixIsNotRegistered() {
        final String iri = "dc:description";
        assertEquals(iri, resolver.resolveFullIri(iri));
    }

    @Test
    public void resolveReturnsPreregisteredIri() {
        assertEquals(CommonVocabulary.RDFS_LABEL, resolver.resolveFullIri("rdfs:label"));
    }

    @Test
    public void resolveReturnsIriBasedOnRegisteredNamespace() {
        resolver.registerNamespace("dc", "http://purl.org/dc/elements/1.1/");
        final String iri = "dc:description";
        assertEquals(CommonVocabulary.DC_DESCRIPTION, resolver.resolveFullIri(iri));
    }

    @Test
    public void resolveReturnsOriginalIriWhenPrefixIsInvalid() {
        final String invalidIri = "prefixWithoutLocalName";
        assertEquals(invalidIri, resolver.resolveFullIri(invalidIri));
    }
}