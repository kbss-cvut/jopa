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
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.EntityGraphImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.Subgraph;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.util.AxiomBasedLoadingConfigGroup;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.emptyCollectionOf;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AttributeBasedRowsToAxiomsQueryResultLoaderTest {

    @Mock
    private UnitOfWork uow;

    @Mock
    private MetamodelImpl metamodel;

    private MetamodelMocks metamodelMocks;

    private final Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        when(uow.getMetamodel()).thenReturn(metamodel);
    }

    @Test
    void loadEntityInstanceReadsRowIntoAxiomsAndLoadsEntityFromThem() throws OntoDriverException {
        final EntityGraph<OWLClassA> fetchGraph = createFetchGraph();
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut = createOwlClassASut(fetchGraph);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(Set.of());
        final List<ResultRow> resultRows = Stream.concat(mockResultRows(instance).stream(), mockResultRows(
                                                         Generators.generateOwlClassAInstance()).stream())
                                                 .toList();
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), any(AxiomBasedLoadingConfigGroup.class))).thenReturn(instance);
        final Optional<OWLClassA> result = resultRows.stream().map(sut::loadResult).filter(Optional::isPresent)
                                                     .map(Optional::get).findFirst();
        assertTrue(result.isPresent());
        assertEquals(instance, result.get());
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(new AxiomBasedLoadingConfigGroup<>(instance.getUri(), descriptor, fetchGraph)));
    }

    private EntityGraph<OWLClassA> createFetchGraph() {
        final IdentifiableEntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        final EntityGraph<OWLClassA> fetchGraph = new EntityGraphImpl<>(et, metamodel);
        fetchGraph.addAttributeNodes(et.getAttributes().toArray(Attribute[]::new));
        return fetchGraph;
    }

    private static List<ResultRow> mockResultRows(OWLClassA instance) throws OntoDriverException {
        final List<String> allTypes = Stream.concat(Stream.of(OWLClassA.getClassIri()), instance.getTypes().stream())
                                            .toList();
        final ResultRow row = mock(ResultRow.class);
        when(row.getColumnCount()).thenReturn(3);
        when(row.getColumnNames()).thenReturn(List.of("x", "x_stringAttribute", "x_types"));
        when(row.getObject(0, URI.class)).thenReturn(instance.getUri());
        when(row.isBound("x")).thenReturn(true);
        when(row.getObject("x", URI.class)).thenReturn(instance.getUri());
        if (instance.getStringAttribute() != null) {
            when(row.isBound("x_stringAttribute")).thenReturn(true);
            when(row.getObject("x_stringAttribute")).thenReturn(instance.getStringAttribute());
        }
        when(row.isBound("x_types")).thenReturn(true);
        when(row.getString("x_types")).thenReturn(String.join(AttributeEnumeratingSparqlAssemblyModifier.GROUP_CONCAT_SEPARATOR, allTypes));
        return List.of(row);
    }

    @Test
    void loadLastPendingLoadsLastPendingEntity() throws Exception {
        final EntityGraph<OWLClassA> fetchGraph = createFetchGraph();
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut = createOwlClassASut(fetchGraph);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(Set.of());
        final List<ResultRow> resultRows = mockResultRows(instance);
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), any(AxiomBasedLoadingConfigGroup.class))).thenReturn(instance);
        resultRows.forEach(row -> {
            final Optional<OWLClassA> opt = sut.loadResult(row);
            assertFalse(opt.isPresent());
        });
        final Optional<OWLClassA> result = sut.loadLastPending();
        assertTrue(result.isPresent());
        assertEquals(instance, result.get());
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), eq(new AxiomBasedLoadingConfigGroup<>(instance.getUri(), descriptor, fetchGraph)));
    }

    private AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> createOwlClassASut(EntityGraph<OWLClassA> fetchGraph) {
        final List<QueryVariableMapping> mappings = List.of(
                new QueryVariableMapping("x", "x_stringAttribute", metamodelMocks.forOwlClassA().stringAttribute()),
                new QueryVariableMapping("x", "x_types", metamodelMocks.forOwlClassA().typesSpec())
        );
        return new AttributeBasedRowsToAxiomsQueryResultLoader<>(uow, OWLClassA.class, descriptor, fetchGraph, () -> mappings);
    }

    @Test
    void loadResultDoesNotDuplicateAlreadyPresentAttributes() throws Exception {
        final EntityGraph<OWLClassA> fetchGraph = createFetchGraph();
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut = createOwlClassASut(fetchGraph);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(Set.of());
        final List<ResultRow> resultRows = mockResultRows(instance);
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), any(AxiomBasedLoadingConfigGroup.class))).thenReturn(instance);
        resultRows.forEach(row -> {
            final Optional<OWLClassA> opt = sut.loadResult(row);
            assertFalse(opt.isPresent());
        });
        final Optional<OWLClassA> result = sut.loadLastPending();
        assertTrue(result.isPresent());
        final ArgumentCaptor<Collection<Axiom<?>>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), captor.capture(), eq(new AxiomBasedLoadingConfigGroup<>(instance.getUri(), descriptor, fetchGraph)));
        assertEquals(1, captor.getValue().stream()
                              .filter(ax -> ax.getValue().equals(new Value<>(instance.getStringAttribute()))).count());
    }

    @Test
    void loadResultHandlesUnboundAttributeVariables() throws Exception {
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut = createOwlClassASut(createFetchGraph());
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(Set.of());
        instance.setStringAttribute(null);
        final List<ResultRow> resultRows = mockResultRows(instance);
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), any(AxiomBasedLoadingConfigGroup.class))).thenReturn(instance);
        resultRows.forEach(row -> {
            final Optional<OWLClassA> opt = sut.loadResult(row);
            assertFalse(opt.isPresent());
        });
        final Optional<OWLClassA> result = sut.loadLastPending();
        assertTrue(result.isPresent());
        assertNull(result.get().getStringAttribute());
        assertThat(result.get().getTypes(), anyOf(nullValue(), emptyCollectionOf(String.class)));
    }

    @Test
    void loadResultSupportsFetchGraphWithMultipleEntities() throws Exception {
        final EntityGraph<OWLClassD> fetchGraph = new EntityGraphImpl<>(metamodel.entity(OWLClassD.class), metamodel);
        final Subgraph<OWLClassA> sg = fetchGraph.addSubgraph("owlClassA");
        sg.addAttributeNodes("stringAttribute");
        final List<QueryVariableMapping> owlClassDMappings = List.of(
                new QueryVariableMapping("x", "x_types", null),
                new QueryVariableMapping("x", "x_owlClassA", metamodelMocks.forOwlClassD().owlClassAAtt()),
                new QueryVariableMapping("x_owlClassA", "x_owlClassA_stringAttribute", metamodelMocks.forOwlClassA()
                                                                                                     .stringAttribute()),
                new QueryVariableMapping("x_owlClassA", "x_owlClassA_types", metamodelMocks.forOwlClassA().typesSpec())
        );
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassD> sut = new AttributeBasedRowsToAxiomsQueryResultLoader<>(
                uow, OWLClassD.class, descriptor, fetchGraph, () -> owlClassDMappings);
        final OWLClassD instance = new OWLClassD(Generators.createIndividualIdentifier());
        instance.setOwlClassA(Generators.generateOwlClassAInstance());
        final List<ResultRow> resultRows = mockResultRows(instance);
        resultRows.forEach(row -> {
            final Optional<OWLClassD> opt = sut.loadResult(row);
            assertFalse(opt.isPresent());
        });
        sut.loadLastPending();
        final ArgumentCaptor<Collection<Axiom<?>>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(uow).readObjectFromAxioms(eq(OWLClassD.class), captor.capture(), eq(new AxiomBasedLoadingConfigGroup<>(instance.getUri(), descriptor, fetchGraph)));
        assertThat(captor.getValue(), hasItem(
                new AxiomImpl<>(NamedResource.create(instance.getUri()), Assertion.createClassAssertion(false), new Value<>(URI.create(Vocabulary.c_OwlClassD)))));
        assertThat(captor.getValue(), hasItem(new AxiomImpl<>(NamedResource.create(instance.getUri()), Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_h_hasA), false), new Value<>(instance.getOwlClassA()
                                                                                                                                                                                                            .getUri()))));
        assertThat(captor.getValue(), hasItem(new AxiomImpl<>(NamedResource.create(instance.getOwlClassA()
                                                                                           .getUri()), Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_a_stringAttribute), false), new Value<>(instance.getOwlClassA()
                                                                                                                                                                                                                     .getStringAttribute()))));
        assertThat(captor.getValue(), hasItem(new AxiomImpl<>(NamedResource.create(instance.getOwlClassA()
                                                                                           .getUri()), Assertion.createClassAssertion(false), new Value<>(URI.create(Vocabulary.c_OwlClassA)))));

        instance.getOwlClassA().getTypes()
                .forEach(t -> assertThat(captor.getValue(), hasItem(new AxiomImpl<>(NamedResource.create(instance.getOwlClassA()
                                                                                                                 .getUri()), Assertion.createClassAssertion(false), new Value<>(URI.create(t))))));
    }

    private static List<ResultRow> mockResultRows(OWLClassD instance) throws Exception {
        final List<String> aTypes = Stream.concat(Stream.of(OWLClassA.getClassIri()), instance.getOwlClassA().getTypes()
                                                                                              .stream()).toList();
        final List<ResultRow> rows = new ArrayList<>(aTypes.size());
        for (String type : aTypes) {
            final ResultRow row = mock(ResultRow.class);
            when(row.getColumnCount()).thenReturn(5);
            when(row.getColumnNames()).thenReturn(List.of("x", "x_types", "x_owlClassA", "x_owlClassA_stringAttribute", "x_owlClassA_types"));
            when(row.getObject(0, URI.class)).thenReturn(instance.getUri());
            when(row.isBound("x")).thenReturn(true);
            when(row.getObject("x", URI.class)).thenReturn(instance.getUri());
            when(row.isBound("x_types")).thenReturn(true);
            when(row.getObject("x_types")).thenReturn(URI.create(Vocabulary.c_OwlClassD));
            when(row.isBound("x_owlClassA")).thenReturn(true);
            when(row.getObject("x_owlClassA")).thenReturn(instance.getOwlClassA().getUri());
            when(row.getObject("x_owlClassA", URI.class)).thenReturn(instance.getOwlClassA().getUri());
            when(row.isBound("x_owlClassA_stringAttribute")).thenReturn(true);
            when(row.getObject("x_owlClassA_stringAttribute")).thenReturn(instance.getOwlClassA().getStringAttribute());
            when(row.isBound("x_owlClassA_types")).thenReturn(true);
            when(row.getString("x_owlClassA_types")).thenReturn(type);
            rows.add(row);
        }
        return rows;
    }

    @Test
    void loadResultSupportsGroupConcatenatedTypes() throws Exception {
        final EntityGraph<OWLClassA> fetchGraph = new EntityGraphImpl<>(metamodelMocks.forOwlClassA()
                                                                                      .entityType(), metamodel);
        fetchGraph.addAttributeNodes("stringAttribute");
        final AttributeBasedRowsToAxiomsQueryResultLoader<OWLClassA> sut = new AttributeBasedRowsToAxiomsQueryResultLoader<>(uow, OWLClassA.class, descriptor, fetchGraph);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final ResultRow row = mock(ResultRow.class);
        when(row.getColumnCount()).thenReturn(3);
        when(row.getColumnNames()).thenReturn(List.of("x", "x_stringAttribute", "x_types"));
        when(row.getObject(0, URI.class)).thenReturn(instance.getUri());
        when(row.isBound("x")).thenReturn(true);
        when(row.getObject("x", URI.class)).thenReturn(instance.getUri());
        when(row.isBound("x_stringAttribute")).thenReturn(true);
        when(row.getObject("x_stringAttribute")).thenReturn(instance.getStringAttribute());
        when(row.isBound("x_types")).thenReturn(true);
        final Stream<String> aTypes = Stream.concat(Stream.of(Vocabulary.c_OwlClassA), instance.getTypes().stream());
        when(row.getString("x_types")).thenReturn(aTypes.collect(Collectors.joining(AttributeEnumeratingSparqlAssemblyModifier.GROUP_CONCAT_SEPARATOR)));
        when(uow.readObjectFromAxioms(eq(OWLClassA.class), anyCollection(), any(AxiomBasedLoadingConfigGroup.class))).thenReturn(instance);

        final Optional<OWLClassA> intermediate = sut.loadResult(row);
        assertFalse(intermediate.isPresent());
        final Optional<OWLClassA> result = sut.loadLastPending();
        assertTrue(result.isPresent());
        final ArgumentCaptor<Collection<Axiom<?>>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(uow).readObjectFromAxioms(eq(OWLClassA.class), captor.capture(), any(AxiomBasedLoadingConfigGroup.class));
        captor.getValue().forEach(ax -> assertEquals(instance.getUri(), ax.getSubject().getIdentifier()));
        instance.getTypes().forEach(type -> assertTrue(captor.getValue().stream()
                                                             .anyMatch(ax -> ax.getValue().stringValue()
                                                                               .equals(type))));
        assertTrue(captor.getValue().stream()
                         .anyMatch(ax -> ax.getValue().stringValue().equals(Vocabulary.c_OwlClassA)));
        assertTrue(captor.getValue().stream()
                         .anyMatch(ax -> ax.getValue().stringValue().equals(instance.getStringAttribute())));
    }

}
