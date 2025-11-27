package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.Sparql11QueryParser;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalToCompressingWhiteSpace;
import static org.mockito.Mockito.mock;

class AttributeEnumeratingSparqlAssemblyModifierTest {

    private static MetamodelMocks metamodelMocks;

    private final ParameterValueFactory valueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private final Sparql11QueryParser parser = new Sparql11QueryParser(valueFactory);

    @BeforeAll
    static void setUpBeforeAll() throws Exception {
        metamodelMocks = new MetamodelMocks();
    }

    @Test
    void modifyAddsOptionalTriplePatternForEntityAttributesAndProjectsAttributeValues() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = new AttributeEnumeratingSparqlAssemblyModifier(metamodelMocks.forOwlClassD()
                                                                                                                            .entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xowlClassA WHERE { ?x a ?type . " +
                "OPTIONAL { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> ?xowlClassA } }"));
    }

    @Test
    void modifyAddsTypesOptionalTriplePatternAndProjection() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = new AttributeEnumeratingSparqlAssemblyModifier(metamodelMocks.forOwlClassA()
                                                                                                                            .entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xstringAttribute ?xtypes WHERE { ?x a ?type . " +
                "OPTIONAL { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?xstringAttribute } " +
                "OPTIONAL { ?x a ?xtypes } }"));
    }
}
