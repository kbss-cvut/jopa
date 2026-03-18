/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
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
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalToCompressingWhiteSpace;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
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
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?x_owlClassA ?x_types WHERE { ?x a ?type . " +
                "?x a ?x_types . " +
                "OPTIONAL { ?x " + strUri(Vocabulary.p_h_hasA) + " ?x_owlClassA . } }"));
    }

    private static String strUri(String uri) {
        return IdentifierTransformer.stringifyIri(uri);
    }

    private <X> AttributeEnumeratingSparqlAssemblyModifier createSut(IdentifiableEntityType<X> et) {
        return createSut(et, new EntityDescriptor());
    }

    private <X> AttributeEnumeratingSparqlAssemblyModifier createSut(IdentifiableEntityType<X> et,
                                                                     Descriptor descriptor) {
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        metamodelMocks.setMocks(metamodel);
        return new AttributeEnumeratingSparqlAssemblyModifier(metamodel, et, descriptor, null, connectionWrapper);
    }

    @Test
    void modifyAddsOptionalTriplePatternForEntityAttributesAndProjectsAttributeValues() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?x_stringAttribute ?x_types WHERE { ?x a ?type . " +
                "?x a ?x_types . " +
                "OPTIONAL { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?x_stringAttribute . } }"));
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
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?x_stringAttribute ?x_types WHERE { ?x a ?type . " +
                "GRAPH <" + ctx + "> { ?x a ?x_types . } " +
                "OPTIONAL { GRAPH <" + ctx + "> { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?x_stringAttribute . } } }"));
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
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?x_stringAttribute ?x_types WHERE { ?x a ?type . " +
                "?x a ?x_types . " +
                "OPTIONAL { GRAPH <" + ctx + "> { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?x_stringAttribute . } } } "));
    }

    @Test
    void modifyAddsOptionalTriplePatternsForSubtypeAttributes() {
        when(metamodelMocks.forOwlClassR().rOwlClassAAtt().getFetchType()).thenReturn(FetchType.EAGER);
        try {
            final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
            final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassS()
                                                                                           .entityType());
            holder.setAssemblyModifier(sut);

            final String result = holder.assembleQuery();
            assertThat(result, containsString(Vocabulary.P_R_STRING_ATTRIBUTE));
            assertThat(result, containsString(Vocabulary.P_HAS_A));
        } finally {
            when(metamodelMocks.forOwlClassR().rOwlClassAAtt().getFetchType()).thenReturn(FetchType.LAZY);
        }
    }

    @Test
    void modifyAddsTriplePatternWithoutOptionalWhenMinimumParticipationConstraintIsPresentAndGreaterThanZero() {
        when(metamodelMocks.forOwlClassJ().setAttribute().getFetchType()).thenReturn(FetchType.EAGER);
        try {
            final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
            final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassJ()
                                                                                           .entityType());
            holder.setAssemblyModifier(sut);

            final String result = holder.assembleQuery();
            assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?x_owlClassA ?x_types WHERE { ?x a ?type . " +
                    "?x " + strUri(Vocabulary.p_h_hasA) + " ?x_owlClassA . " +
                    "?x a ?x_types . }"));
        } finally {
            when(metamodelMocks.forOwlClassJ().setAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        }
    }

    @Test
    void modifyExcludesLazilyLoadedAttributesFromFetching() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassJ().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, equalToCompressingWhiteSpace("SELECT ?x ?x_types WHERE { ?x a ?type . " +
                "?x a ?x_types . }"));
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
        assertThat(result, containsString("OPTIONAL { ?x " + strUri(Vocabulary.p_f_stringAttribute) + " ?x_secondStringAttribute . }"));
    }
}
