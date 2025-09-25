package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class EntityLoadingSparqlAssemblyModifierTest {

    private final ParameterValueFactory valueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private Sparql11QueryParser parser;

    private final EntityLoadingSparqlAssemblyModifier sut = new EntityLoadingSparqlAssemblyModifier();

    @BeforeEach
    void setUp() {
        final UnitOfWork uowMock = mock(UnitOfWork.class);
        when(uowMock.getConfiguration()).thenReturn(new Configuration());
        this.parser = new Sparql11QueryParser(valueFactory, new EntityLoadingOptimizer(uowMock));
    }

    @Test
    void modifyAddsUnboundPropertyAndValuePatternAndProjectsPropertyAndValueForNamedParameter() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        holder.addAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertEquals("SELECT ?x ?xP ?xV WHERE { ?x a ?type . ?x ?xP ?xV . }", result);
    }

    @Test
    void modifyAddsUnboundPropertyAndValuePatternAndProjectsPropertyAndValueForPositionalParameter() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT $1 WHERE { $1 a ?type . }");
        holder.addAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertEquals("SELECT $1 ?1P ?1V WHERE { $1 a ?type . $1 ?1P ?1V . }", result);
    }
}
