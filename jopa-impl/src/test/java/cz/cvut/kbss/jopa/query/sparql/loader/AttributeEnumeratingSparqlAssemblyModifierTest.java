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
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalToCompressingWhiteSpace;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AttributeEnumeratingSparqlAssemblyModifierTest {

    private static MetamodelMocks metamodelMocks;

    @Mock
    private ConnectionWrapper connectionWrapper;

    private final ParameterValueFactory valueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private final Sparql11QueryParser parser = new Sparql11QueryParser(valueFactory);

    @BeforeAll
    static void setUpBeforeAll() throws Exception {
        metamodelMocks = new MetamodelMocks();
    }

    @BeforeEach
    void setUp() {
        when(connectionWrapper.getRepositoryMetadata()).thenReturn(() -> "test");
    }

    @Test
    void modifyAlwaysAddsTriplePatternForEntityTypes() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassD().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xowlClassA ?xtypes WHERE { ?x a ?type . " +
                "OPTIONAL { ?x " + strUri(Vocabulary.p_h_hasA) + " ?xowlClassA . } " +
                "?x a ?xtypes . }"));
    }

    private static String strUri(String uri) {
        return IdentifierTransformer.stringifyIri(uri);
    }

    private <X> AttributeEnumeratingSparqlAssemblyModifier createSut(IdentifiableEntityType<X> et) {
        return createSut(et, new EntityDescriptor());
    }

    private <X> AttributeEnumeratingSparqlAssemblyModifier createSut(IdentifiableEntityType<X> et,
                                                                     Descriptor descriptor) {
        return new AttributeEnumeratingSparqlAssemblyModifier(et, descriptor, connectionWrapper);
    }

    @Test
    void modifyAddsOptionalTriplePatternForEntityAttributesAndProjectsAttributeValues() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xstringAttribute ?xtypes WHERE { ?x a ?type . " +
                "OPTIONAL { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?xstringAttribute . } " +
                "?x a ?xtypes . }"));
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
                "OPTIONAL { GRAPH <" + ctx + "> { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?xstringAttribute . } } " +
                "GRAPH <" + ctx + "> { ?x a ?xtypes . } }"));
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
                "OPTIONAL { GRAPH <" + ctx + "> { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?xstringAttribute . } } " +
                "?x a ?xtypes . }"));
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

    @Test
    void modifyAddsTriplePatternWithoutOptionalWhenMinimumParticipationConstraintIsPresentAndGreaterThanZero() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassJ().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?xowlClassA ?xtypes WHERE { ?x a ?type . " +
                "?x " + strUri(Vocabulary.p_h_hasA) + " ?xowlClassA . " +
                "?x a ?xtypes . }"));
    }

    @Test
    void modifyUsesDefaultContextForInferredAttributeWhenRepositoryProductIsGraphDBAndNonDefaultContextIsProvided() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final URI ctx = Generators.createIndividualIdentifier();
        final EntityDescriptor descriptor = new EntityDescriptor(ctx);
        when(connectionWrapper.getRepositoryMetadata()).thenReturn(() -> "GraphDB");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassF()
                                                                                       .entityType(), descriptor);
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, containsString("OPTIONAL { ?x " + strUri(Vocabulary.p_f_stringAttribute) + " ?xsecondStringAttribute . }"));
    }
}
