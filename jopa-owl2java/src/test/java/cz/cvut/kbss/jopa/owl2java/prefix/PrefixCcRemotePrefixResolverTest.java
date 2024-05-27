package cz.cvut.kbss.jopa.owl2java.prefix;

import cz.cvut.kbss.jopa.owl2java.environment.Generator;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.IRI;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PrefixCcRemotePrefixResolverTest {

    private final PrefixCcRemotePrefixResolver sut = new PrefixCcRemotePrefixResolver();

    @Test
    void resolvePrefixReturnsPrefixReturnedByPrefixCcService() {
        final Optional<String> result = sut.resolvePrefix(IRI.create("http://xmlns.com/foaf/0.1/"));
        assertTrue(result.isPresent());
        assertEquals("foaf", result.get());
    }

    @Test
    void resolvePrefixReturnsEmptyOptionalWhenPrefixCcServiceReturnsNotFound() {
        final Optional<String> result = sut.resolvePrefix(IRI.create(Generator.generateUri()));
        assertTrue(result.isEmpty());
    }
}
