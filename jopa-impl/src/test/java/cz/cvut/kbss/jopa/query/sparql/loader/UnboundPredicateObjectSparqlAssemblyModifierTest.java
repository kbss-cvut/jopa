package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.Sparql11QueryParser;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalToCompressingWhiteSpace;
import static org.mockito.Mockito.mock;

class UnboundPredicateObjectSparqlAssemblyModifierTest {

    private final ParameterValueFactory valueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private final Sparql11QueryParser parser = new Sparql11QueryParser(valueFactory);

    private final UnboundPredicateObjectSparqlAssemblyModifier sut = new UnboundPredicateObjectSparqlAssemblyModifier();

    @Test
    void modifyAddsUnboundPropertyAndValuePatternAndProjectsPropertyAndValueForNamedParameter() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xP ?xV WHERE { ?x a ?type . ?x ?xP ?xV . }"));
    }

    @Test
    void modifyAddsUnboundPropertyAndValuePatternAndProjectsPropertyAndValueForPositionalParameter() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT $1 WHERE { $1 a ?type . }");
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT $1 ?1P ?1V WHERE { $1 a ?type . $1 ?1P ?1V . }"));
    }

    @Test
    void modifyInsertsDotBeforePropertyAndValuePatternWhenLastTriplePatternDoesNotEndWithDot() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type }");
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xP ?xV WHERE { ?x a ?type . ?x ?xP ?xV . }"));
    }
}
