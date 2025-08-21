package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.*;

class RdfContainerTriplePatternEnhancerTest {

    @Test
    void enhanceDoesNotGenerateInvalidSparqlVariableNames() {
        final RdfContainerTriplePatternEnhancer sut = new RdfContainerTriplePatternEnhancer();
        final String predicate = "?negative";
        final List<String> result = sut.getTriplePatterns("?x", predicate, Generators.createIndividualIdentifier().toString());
        result.forEach(tp -> {
            assertThat(tp, not(containsString("?rdfContainer-")));
            assertThat(tp, not(containsString("?hasElement-")));
        });
    }
}
