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

import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.EntityGraphImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.Sparql11QueryParser;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;

import static cz.cvut.kbss.jopa.query.sparql.loader.AttributeEnumeratingSparqlAssemblyModifier.GROUP_CONCAT_SEPARATOR;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AttributeEnumeratingSparqlAssemblyModifierTest {

    private static MetamodelMocks metamodelMocks;

    @Mock
    private ConnectionWrapper connectionWrapper;

    @Mock
    private MetamodelImpl metamodel;

    private final ParameterValueFactory valueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private final Sparql11QueryParser parser = new Sparql11QueryParser(valueFactory);

    @BeforeAll
    static void setUpBeforeAll() throws Exception {
        metamodelMocks = new MetamodelMocks();
    }

    @BeforeEach
    void setUp() {
        when(connectionWrapper.getRepositoryMetadata()).thenReturn(() -> "test");
        metamodelMocks.setMocks(metamodel);
    }

    @Test
    void modifyAlwaysAddsTriplePatternForEntityType() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassD().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, projectsVariable("x"));
        assertThat(result, containsString("?x a " + strUri(Vocabulary.c_OwlClassD) + " ."));
    }

    private static ProjectsVariableMatcher projectsVariable(String variable) {
        return new ProjectsVariableMatcher(variable);
    }

    private static String strUri(String uri) {
        return IdentifierTransformer.stringifyIri(uri);
    }

    private <X> AttributeEnumeratingSparqlAssemblyModifier createSut(IdentifiableEntityType<X> et) {
        return createSut(et, new EntityDescriptor(), null);
    }

    private <X> AttributeEnumeratingSparqlAssemblyModifier createSut(IdentifiableEntityType<X> et,
                                                                     Descriptor descriptor, EntityGraph<?> fetchGraph) {
        return new AttributeEnumeratingSparqlAssemblyModifier(metamodel, et, descriptor, fetchGraph, connectionWrapper);
    }

    @Test
    void modifyAddsOptionalTriplePatternForEntityAttributesAndProjectsAttributeValues() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, projectsVariable("x"));
        assertThat(result, projectsVariable("x_stringAttribute"));
        assertThat(result, containsString("OPTIONAL { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?x_stringAttribute . }"));
    }

    @Test
    void modifyAddsOptionalTriplePatternWithGraphWhenDescriptorWithNonDefaultContextIsProvided() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final URI ctx = Generators.createIndividualIdentifier();
        final EntityDescriptor descriptor = new EntityDescriptor(ctx);
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA()
                                                                                       .entityType(), descriptor, null);
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, containsString("GRAPH <" + ctx + "> { ?x a ?x_types . }"));
        assertThat(result, containsString("OPTIONAL { GRAPH <" + ctx + "> { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?x_stringAttribute . } }"));
    }

    @Test
    void modifyAddsOptionalTriplePatternWithAttributeSpecifiedContextResolvedFromDescriptor() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final URI ctx = Generators.createIndividualIdentifier();
        final EntityDescriptor descriptor = new EntityDescriptor();
        descriptor.addAttributeContext(metamodelMocks.forOwlClassA().entityType().getAttribute("stringAttribute"), ctx);
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA()
                                                                                       .entityType(), descriptor, null);
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, containsString("?x a ?x_types ."));
        assertThat(result, containsString("OPTIONAL { GRAPH <" + ctx + "> { ?x " + strUri(Vocabulary.p_a_stringAttribute) + " ?x_stringAttribute . } }"));
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
            assertThat(result, containsString("?x " + strUri(Vocabulary.p_h_hasA) + " ?x_owlClassA . "));
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
        assertThat(result, not(projectsVariable("x_owlClassA")));
        assertThat(result, not(containsString("?x " + strUri(Vocabulary.p_h_hasA) + " ?x_owlClassA . ")));
    }

    @Test
    void modifyUsesDefaultContextForInferredAttributeWhenRepositoryProductIsGraphDBAndNonDefaultContextIsProvided() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final URI ctx = Generators.createIndividualIdentifier();
        final EntityDescriptor descriptor = new EntityDescriptor(ctx);
        when(connectionWrapper.getRepositoryMetadata()).thenReturn(() -> "GraphDB");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassF()
                                                                                       .entityType(), descriptor, null);
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, containsString("OPTIONAL { ?x " + strUri(Vocabulary.p_f_stringAttribute) + " ?x_secondStringAttribute . }"));
    }

    @Test
    void modifyUsesGroupConcatForPluralAssociationAttributesToReduceRowCountBlowup() {
        // Just for this test
        when(metamodelMocks.forOwlClassA().entityType().getTypes()).thenReturn(null);
        try {
            final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
            final EntityGraph<OWLClassJ> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassJ()
                                                                                          .entityType(), metamodel);
            fetchGraph.addAttributeNodes("owlClassA");
            final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassJ()
                                                                                           .entityType(), new EntityDescriptor(), fetchGraph);
            holder.setAssemblyModifier(sut);

            final String result = holder.assembleQuery();
            assertThat(result, containsString("GROUP_CONCAT(DISTINCT ?x_owlClassA; SEPARATOR='" + GROUP_CONCAT_SEPARATOR + "') AS ?x_owlClassA"));
            assertThat(result, containsString("GROUP BY ?x"));
        } finally {
            // Reset
            when(metamodelMocks.forOwlClassA().entityType()
                               .getTypes()).thenReturn((TypesSpecification) metamodelMocks.forOwlClassA().typesSpec());
        }
    }

    @Test
    void modifyUsesGroupConcatForTypesToReduceRowCountBlowup() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassA().entityType());
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, containsString("GROUP_CONCAT(DISTINCT ?x_types; SEPARATOR='" + GROUP_CONCAT_SEPARATOR + "') AS ?x_types"));
        assertThat(result, containsString("GROUP BY ?x ?x_stringAttribute"));
    }

    @Test
    void modifyGroupConcatenatesOnlyMostSpecificEligibleAttributeToPreserveGrouping() {
        final TokenStreamSparqlQueryHolder holder = parser.parseQuery("SELECT ?x WHERE { ?x a ?type . }");
        final EntityGraph<OWLClassJ> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassJ()
                                                                                      .entityType(), metamodel);
        fetchGraph.addSubgraph("owlClassA");
        final AttributeEnumeratingSparqlAssemblyModifier sut = createSut(metamodelMocks.forOwlClassJ()
                                                                                       .entityType(), new EntityDescriptor(), fetchGraph);
        holder.setAssemblyModifier(sut);

        final String result = holder.assembleQuery();
        assertThat(result, not(containsString("GROUP_CONCAT(DISTINCT ?x_owlClassA; SEPARATOR='" + GROUP_CONCAT_SEPARATOR + "') AS ?x_owlClassA")));
        assertThat(result, containsString("GROUP_CONCAT(DISTINCT ?x_owlClassA_types; SEPARATOR='" + GROUP_CONCAT_SEPARATOR + "') AS ?x_owlClassA_types"));
        assertThat(result, containsString("GROUP BY ?x ?x_owlClassA"));
    }

    /**
     * Simplified Hamcrest matcher that checks if a SPARQL query projects a specific variable. The matcher extracts the
     * projection part of the query (everything before the WHERE clause) and checks if it contains the specified
     * variable prefixed with '?'.
     */
    private static class ProjectsVariableMatcher extends TypeSafeMatcher<String> {
        private final String variable;

        public ProjectsVariableMatcher(String variable) {
            this.variable = variable;
        }

        @Override
        protected boolean matchesSafely(String query) {
            final String projectionPart = query.substring(0, query.indexOf("WHERE"));
            return projectionPart.contains("?" + variable);
        }

        @Override
        public void describeTo(Description description) {
            description.appendText("a SPARQL query that projects variable ?" + variable);
        }
    }
}
