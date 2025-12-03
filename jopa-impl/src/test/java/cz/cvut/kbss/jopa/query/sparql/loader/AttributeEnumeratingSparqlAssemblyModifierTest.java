package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.Sparql11QueryParser;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
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
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassD().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xowlClassA WHERE { ?x a ?type . " +
                "OPTIONAL { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> ?xowlClassA } }"));
    }

    private static <X> AttributeEnumeratingSparqlAssemblyModifier createSut(IdentifiableEntityType<X> et) {
        return createSut(et, new EntityDescriptor());
    }

    private static <X> AttributeEnumeratingSparqlAssemblyModifier createSut(IdentifiableEntityType<X> et,
                                                                            Descriptor descriptor) {
        return new AttributeEnumeratingSparqlAssemblyModifier(et, descriptor);
    }

    @Test
    void modifyAddsTypesOptionalTriplePatternAndProjection() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xstringAttribute ?xtypes WHERE { ?x a ?type . " +
                "OPTIONAL { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?xstringAttribute } " +
                "OPTIONAL { ?x a ?xtypes } }"));
    }

    @Test
    void modifyAddsOptionalTriplePatternWithGraphWhenDescriptorWithNonDefaultContextIsProvided() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final URI ctx = Generators.createIndividualIdentifier();
        final EntityDescriptor descriptor = new EntityDescriptor(ctx);
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA()
                                                                                       .entityType(), descriptor);
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xstringAttribute ?xtypes WHERE { ?x a ?type . " +
                "OPTIONAL { GRAPH <" + ctx + "> { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?xstringAttribute } } " +
                "OPTIONAL { GRAPH <" + ctx + "> { ?x a ?xtypes } } }"));
    }

    @Test
    void modifyAddsOptionalTriplePatternWithAttributeSpecifiedContextResolvedFromDescriptor() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final URI ctx = Generators.createIndividualIdentifier();
        final EntityDescriptor descriptor = new EntityDescriptor();
        descriptor.addAttributeContext(metamodelMocks.forOwlClassA().entityType().getAttribute("stringAttribute"), ctx);
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA()
                                                                                       .entityType(), descriptor);
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xstringAttribute ?xtypes WHERE { ?x a ?type . " +
                "OPTIONAL { GRAPH <" + ctx + "> { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?xstringAttribute } } " +
                "OPTIONAL { ?x a ?xtypes } }"));
    }

    @Test
    void modifyAddsOptionalTriplePatternsForSubtypeAttributes() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassS().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, containsString(Vocabulary.P_R_STRING_ATTRIBUTE));
        assertThat(result, containsString(Vocabulary.P_HAS_A));
    }
}
