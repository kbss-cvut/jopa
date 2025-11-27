package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.Sparql11QueryParser;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalToCompressingWhiteSpace;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class UnboundPredicateObjectSparqlAssemblyModifierTest {

    private final ParameterValueFactory valueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private Sparql11QueryParser parser;

    private final UnboundPredicateObjectSparqlAssemblyModifier sut = new UnboundPredicateObjectSparqlAssemblyModifier();

    @BeforeEach
    void setUp() {
        final UnitOfWork uowMock = mock(UnitOfWork.class);
        when(uowMock.getConfiguration()).thenReturn(new Configuration());
        this.parser = new Sparql11QueryParser(valueFactory);
    }

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
