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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.EntityGraphImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.Sparql11QueryParser;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.RepositoryMetadata;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Set;

import static cz.cvut.kbss.jopa.utils.IdentifierTransformer.stringifyIri;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class SparqlQueryResultLoadingOptimizerTest {

    @Mock
    private UnitOfWork uow;

    @Mock
    private ConnectionWrapper connectionWrapper;

    private Sparql11QueryParser parser;

    @BeforeEach
    void setUp() {
        this.parser = new Sparql11QueryParser(new ParameterValueFactory(uow));
    }

    @Test
    void optimizeQueryAssemblySetsUnboundPredicateObjectAssemblyModifierWhenQueryIsSelectAndResultClassIsEntityTypeWithProperties() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type }"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassB.class)).thenReturn(true);
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        when(uow.getMetamodel()).thenReturn(metamodel);
        final IdentifiableEntityType<OWLClassB> et = mock(IdentifiableEntityType.class);
        when(et.getProperties()).thenReturn(mock(PropertiesSpecification.class));
        when(metamodel.entity(OWLClassB.class)).thenReturn(et);
        sut.optimizeQueryAssembly(OWLClassB.class, new EntityDescriptor(), null);
        verify(qh).setAssemblyModifier(any(UnboundPredicateObjectSparqlAssemblyModifier.class));
    }

    @Test
    void optimizeQueryAssemblySetsAttributeEnumeratingAssemblyModifierWhenQueryIsSelectAndResultClassIsEntityTypeWithoutProperties() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type }"));
        when(connectionWrapper.getRepositoryMetadata()).thenReturn(mock(RepositoryMetadata.class));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        when(uow.getMetamodel()).thenReturn(metamodel);
        final IdentifiableEntityType<OWLClassA> et = mock(IdentifiableEntityType.class);
        when(et.getProperties()).thenReturn(null);
        when(metamodel.entity(OWLClassA.class)).thenReturn(et);
        sut.optimizeQueryAssembly(OWLClassA.class, new EntityDescriptor(), null);
        verify(qh).setAssemblyModifier(any(AttributeEnumeratingSparqlAssemblyModifier.class));
    }

    @Test
    void optimizeQueryAssemblyDoesNotOptimizeQueryWhenResultClassIsNotEntity() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type }"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        sut.optimizeQueryAssembly(URI.class, new EntityDescriptor(), null);
        verify(qh, never()).setAssemblyModifier(any());
    }

    @Test
    void optimizeQueryAssemblyDoesNotOptimizeQueryWhenLimitIsSet() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type } LIMIT 1"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        sut.optimizeQueryAssembly(OWLClassA.class, new EntityDescriptor(), null);
        verify(qh, never()).setAssemblyModifier(any());
    }

    @Test
    void optimizeQueryAssemblyDoesNotOptimizeQueryWhenOffsetIsSet() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type } OFFSET 1"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        sut.optimizeQueryAssembly(OWLClassA.class, new EntityDescriptor(), null);
        verify(qh, never()).setAssemblyModifier(any());
    }

    @Test
    void optimizeQueryAssemblyOptimizesQueryWhenItContainsGraphClauseAndAttributeBasedOptimizerCanBeUser() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { GRAPH ?g { ?s a ?type } }"));
        when(connectionWrapper.getRepositoryMetadata()).thenReturn(mock(RepositoryMetadata.class));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        when(uow.getMetamodel()).thenReturn(metamodel);
        final IdentifiableEntityType<OWLClassA> et = mock(IdentifiableEntityType.class);
        when(metamodel.entity(OWLClassA.class)).thenReturn(et);
        sut.optimizeQueryAssembly(OWLClassA.class, new EntityDescriptor(), null);
        verify(qh).setAssemblyModifier(any(AttributeEnumeratingSparqlAssemblyModifier.class));
    }

    @Test
    void optimizeQueryAssemblyDoesNotOptimizeQueryWhenProvidedDescriptorSpecifiesMoreThanOneContext() {
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type }"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        sut.optimizeQueryAssembly(OWLClassA.class, new EntityDescriptor(Set.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier())), null);
        verify(qh, never()).setAssemblyModifier(any());
    }

    @Test
    void getQueryResultLoaderReturnsAttributeBasedRowsToAxiomsResultLoaderWhenQueryProjectsMultipleVariablesAndResultTypeIsEntity() {
        final TokenStreamSparqlQueryHolder qh = parser.parseQuery("SELECT ?x ?stringAttribute ?types WHERE {" +
                "?x a " + stringifyIri(Vocabulary.c_OwlClassA) + " ;" +
                stringifyIri(Vocabulary.p_a_stringAttribute) + " ?stringAttribute ;" +
                "a ?types . }");
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        when(uow.getMetamodel()).thenReturn(metamodel);
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.disableOptimization();
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);
        final QueryResultLoader<OWLClassA> result = sut.getQueryResultLoader(OWLClassA.class, new EntityDescriptor(), null);
        assertInstanceOf(AttributeBasedRowsToAxiomsQueryResultLoader.class, result);
    }

    @Test
    void optimizeQueryAssemblyUsesAttributeEnumeratingSparqlAssemblyModifierForEntityWithSubTypesWhenFetchGraphIsProvided() {
        when(connectionWrapper.getRepositoryMetadata()).thenReturn(() -> "");
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type }"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassS.class)).thenReturn(true);
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        when(uow.getMetamodel()).thenReturn(metamodel);
        final IdentifiableEntityType<OWLClassS> et = mock(IdentifiableEntityType.class);
        when(metamodel.entity(OWLClassS.class)).thenReturn(et);
        final EntityGraph<OWLClassS> fetchGraph = new EntityGraphImpl<>(et, metamodel);
        sut.optimizeQueryAssembly(OWLClassS.class, new EntityDescriptor(), fetchGraph);
        verify(qh).setAssemblyModifier(any(AttributeEnumeratingSparqlAssemblyModifier.class));
    }

    @Test
    void optimizeQueryAssemblyUsesAttributeEnumeratingSparqlAssemblyModifierForEntityWithPropertiesWhenFetchGraphIsProvided() {
        when(connectionWrapper.getRepositoryMetadata()).thenReturn(() -> "");
        final TokenStreamSparqlQueryHolder qh = spy(parser.parseQuery("SELECT ?s WHERE { ?s a ?type }"));
        final SparqlQueryResultLoadingOptimizer sut = new SparqlQueryResultLoadingOptimizer(qh, uow, connectionWrapper);
        sut.enableOptimization();
        when(uow.isEntityType(OWLClassB.class)).thenReturn(true);
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        when(uow.getMetamodel()).thenReturn(metamodel);
        final IdentifiableEntityType<OWLClassB> et = mock(IdentifiableEntityType.class);
        when(metamodel.entity(OWLClassB.class)).thenReturn(et);
        sut.optimizeQueryAssembly(OWLClassB.class, new EntityDescriptor(), null);
        verify(qh).setAssemblyModifier(any(AttributeEnumeratingSparqlAssemblyModifier.class));
    }
}
