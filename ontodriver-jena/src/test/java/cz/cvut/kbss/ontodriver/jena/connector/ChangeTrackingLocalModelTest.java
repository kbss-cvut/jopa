/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.NAMED_GRAPH;
import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.SUBJECT;
import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.TYPE_ONE;
import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.TYPE_TWO;
import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.statement;
import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ChangeTrackingLocalModelTest {

    private final ChangeTrackingLocalModel localModel = new ChangeTrackingLocalModel(false);

    @Test
    public void addStatementsAddsThemIntoDefaultAddModel() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(statement), null);
        final Dataset added = localModel.getAdded();
        assertTrue(added.getDefaultModel().contains(statement));
    }

    @Test
    public void addStatementsWithContextAddsThemIntoContextAddModel() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(statement), NAMED_GRAPH);
        final Dataset added = localModel.getAdded();
        assertTrue(added.getNamedModel(NAMED_GRAPH).contains(statement));
    }

    @Test
    public void removeStatementsAddsThemIntoDefaultRemoveModel() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.removeStatements(Collections.singletonList(statement), null);
        final Dataset removed = localModel.getRemoved();
        assertTrue(removed.getDefaultModel().contains(statement));
    }

    @Test
    public void removeStatementsWithContextAddsThemIntoContextRemoveModel() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.removeStatements(Collections.singletonList(statement), NAMED_GRAPH);
        final Dataset removed = localModel.getRemoved();
        assertTrue(removed.getNamedModel(NAMED_GRAPH).contains(statement));
    }

    @Test
    public void containsReturnsAddedForStatementInAdded() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(statement), null);
        assertEquals(ChangeTrackingLocalModel.Containment.ADDED,
                     localModel.contains(statement.getSubject(), statement.getPredicate(), null, Collections.emptySet()));
    }

    @Test
    public void containsReturnsAddedForStatementsInAddedContext() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(statement), NAMED_GRAPH);
        assertEquals(ChangeTrackingLocalModel.Containment.ADDED,
                     localModel.contains(createResource(SUBJECT), statement.getPredicate(), null, Collections.singleton(NAMED_GRAPH)));
    }

    @Test
    public void containsReturnsRemovedForStatementInRemoved() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.removeStatements(Collections.singletonList(statement), null);
        assertEquals(ChangeTrackingLocalModel.Containment.REMOVED,
                     localModel.contains(statement.getSubject(), statement.getPredicate(), null, Collections.emptySet()));
    }

    @Test
    public void containsReturnsRemovedForStatementInRemovedContext() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.removeStatements(Collections.singletonList(statement), NAMED_GRAPH);
        assertEquals(ChangeTrackingLocalModel.Containment.REMOVED,
                     localModel.contains(statement.getSubject(), statement.getPredicate(), null,
                        Collections.singleton(NAMED_GRAPH)));
    }

    @Test
    public void containsReturnsUnknownForStatementNotInLocalModel() {
        assertEquals(ChangeTrackingLocalModel.Containment.UNKNOWN,
                     localModel.contains(createResource(SUBJECT), null, createResource(TYPE_ONE), Collections.emptySet()));
    }

    @Test
    public void addStatementsRemovesThemFromRemoved() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.removeStatements(Collections.singletonList(statement), null);
        assertTrue(localModel.getRemoved().getDefaultModel()
                             .contains(statement.getSubject(), statement.getPredicate(), statement.getObject()));
        localModel.addStatements(Collections.singletonList(statement), null);
        assertFalse(localModel.getRemoved().getDefaultModel()
                              .contains(statement.getSubject(), statement.getPredicate(), statement.getObject()));
    }

    @Test
    public void addStatementsRemovesThemFromRemoved_Context() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.removeStatements(Collections.singletonList(statement), NAMED_GRAPH);
        assertTrue(localModel.getRemoved().getNamedModel(NAMED_GRAPH)
                             .contains(statement.getSubject(), statement.getPredicate(), statement.getObject()));
        localModel.addStatements(Collections.singletonList(statement), NAMED_GRAPH);
        assertFalse(localModel.getRemoved().getNamedModel(NAMED_GRAPH)
                              .contains(statement.getSubject(), statement.getPredicate(), statement.getObject()));
    }

    @Test
    public void removeStatementsRemovesThemFromAdded() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(statement), null);
        assertTrue(localModel.getAdded().getDefaultModel()
                             .contains(statement.getSubject(), statement.getPredicate(), statement.getObject()));
        localModel.removeStatements(Collections.singletonList(statement), null);
        assertFalse(localModel.getAdded().getDefaultModel()
                              .contains(statement.getSubject(), statement.getPredicate(), statement.getObject()));
    }

    @Test
    public void removeStatementsRemovesThemFromAdded_Context() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(statement), NAMED_GRAPH);
        assertTrue(localModel.getAdded().getNamedModel(NAMED_GRAPH)
                             .contains(statement.getSubject(), statement.getPredicate(), statement.getObject()));
        localModel.removeStatements(Collections.singletonList(statement), NAMED_GRAPH);
        assertFalse(localModel.getAdded().getNamedModel(NAMED_GRAPH)
                              .contains(statement.getSubject(), statement.getPredicate(), statement.getObject()));
    }

    @Test
    public void enhanceStatementsAddsAddedStatements() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(statement), null);
        final Collection<Statement> toEnhance = Collections
                .singletonList(statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO));
        final Collection<Statement> result = localModel
                .enhanceStatements(toEnhance, createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), null,
                        Collections.emptySet());
        assertEquals(2, result.size());
        assertTrue(result.contains(statement));
        assertTrue(result.containsAll(toEnhance));
    }

    @Test
    public void enhanceStatementsRemovesRemovedStatements() {
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.removeStatements(Collections.singletonList(statement), null);
        final Collection<Statement> toEnhance = Collections
                .singletonList(statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE));
        final Collection<Statement> result = localModel
                .enhanceStatements(toEnhance, createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), null,
                        Collections.emptySet());
        assertTrue(result.isEmpty());
    }

    @Test
    public void enhanceStatementsWorksInContext() {
        final Statement added = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(added), NAMED_GRAPH);
        final Statement removed = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO);
        localModel.removeStatements(Collections.singletonList(removed), NAMED_GRAPH);
        final Collection<Statement> toEnhance = Collections
                .singletonList(statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO));
        final Collection<Statement> result = localModel
                .enhanceStatements(toEnhance, createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), null,
                        Collections.singleton(NAMED_GRAPH));
        assertEquals(1, result.size());
        assertTrue(result.contains(added));
    }

    @Test
    public void getContextsGetsNamedGraphsInLocalModel() {
        final Statement added = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        localModel.addStatements(Collections.singletonList(added), NAMED_GRAPH);
        final List<String> result = localModel.getContexts();
        assertTrue(result.contains(NAMED_GRAPH));
    }

    @Test
    public void getContextReturnsEmptyListWhenNoNamedGraphsArePresent() {
        final List<String> contexts = localModel.getContexts();
        assertNotNull(contexts);
        assertTrue(contexts.isEmpty());
    }

    @Test
    public void containsUsesUnionGraphWhenConfigured() {
        final LocalModel model = new ChangeTrackingLocalModel(true);
        final Statement added = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_ONE);
        model.addStatements(Collections.singletonList(added), NAMED_GRAPH);
        assertEquals(ChangeTrackingLocalModel.Containment.ADDED,
                     model.contains(createResource(SUBJECT), null, null, Collections.emptySet()));
    }

    @Test
    public void enhanceUsesUnionGraphWhenConfigured() {
        final LocalModel model = new ChangeTrackingLocalModel(true);
        final Statement removed = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO);
        model.removeStatements(Collections.singletonList(removed), NAMED_GRAPH);
        final Collection<Statement> toEnhance = Collections
                .singletonList(statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO));
        final Collection<Statement> result = model
                .enhanceStatements(toEnhance, createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), null,
                        Collections.emptySet());
        assertTrue(result.isEmpty());
    }

    @Test
    public void removeRemovesStatementsFromAllNamedGraphsWhenDefaultAsUnionIsConfigured() {
        final LocalModel model = new ChangeTrackingLocalModel(true);
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO);
        model.addStatements(Collections.singletonList(statement), NAMED_GRAPH);
        model.removeStatements(Collections.singletonList(statement), null);
        assertTrue(model.getAdded().isEmpty());
    }

    @Test
    public void addToDefaultRemovesStatementsFromRemovedContext() {
        final LocalModel model = new ChangeTrackingLocalModel(true);
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO);
        model.removeStatements(Collections.singletonList(statement), NAMED_GRAPH);
        model.addStatements(Collections.singletonList(statement), null);
        assertTrue(model.getRemoved().isEmpty());
        assertTrue(model.getAdded().getDefaultModel().contains(statement));
    }

    @Test
    void containsReturnsFalseWhenSubjectAndPredicateWereRemovedInLocalModel() {
        final LocalModel sut = new ChangeTrackingLocalModel(true);
        sut.removePropertyValues(Set.of(new SubjectPredicateContext(createResource(SUBJECT),
                createProperty(Vocabulary.RDF_TYPE), Collections.emptySet())));
        assertEquals(ChangeTrackingLocalModel.Containment.REMOVED, sut.contains(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), createResource(TYPE_TWO), Collections.emptySet()));
    }

    @Test
    void containsReturnsFalseWhenSubjectAndPredicateWereRemovedInLocalModelInMatchingContext() {
        final LocalModel sut = new ChangeTrackingLocalModel(true);
        sut.removePropertyValues(Set.of(new SubjectPredicateContext(createResource(SUBJECT),
                createProperty(Vocabulary.RDF_TYPE), Set.of(NAMED_GRAPH))));
        assertEquals(ChangeTrackingLocalModel.Containment.REMOVED, sut.contains(createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), createResource(TYPE_TWO), Set.of(NAMED_GRAPH)));
    }

    @Test
    void enhanceStatementsRemovesStatementsWhoseSubjectPredicateMatchRemoved() {
        final LocalModel sut = new ChangeTrackingLocalModel(true);
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO);
        sut.removePropertyValues(Set.of(new SubjectPredicateContext(createResource(SUBJECT),
                createProperty(Vocabulary.RDF_TYPE), Collections.emptySet())));

        final Collection<Statement> result = sut.enhanceStatements(Set.of(statement), createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), createResource(TYPE_TWO), Collections.emptySet());
        assertTrue(result.isEmpty());
    }

    @Test
    void enhanceStatementsRemovesStatementsWhoseSubjectPredicateAndContextMatchRemoved() {
        final LocalModel sut = new ChangeTrackingLocalModel(true);
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO);
        sut.removePropertyValues(Set.of(new SubjectPredicateContext(createResource(SUBJECT),
                createProperty(Vocabulary.RDF_TYPE), Set.of(NAMED_GRAPH))));

        final Collection<Statement> result = sut.enhanceStatements(Set.of(statement), createResource(SUBJECT), createProperty(Vocabulary.RDF_TYPE), null, Set.of(NAMED_GRAPH));
        assertTrue(result.isEmpty());
    }

    @Test
    void removeStatementsBySubjectAndPredicateRemovesPreviouslyAddedStatementsWithMatchingSubjectPredicate() {
        final LocalModel sut = new ChangeTrackingLocalModel(true);
        final Statement statement = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO);
        sut.addStatements(Collections.singletonList(statement), null);
        sut.removePropertyValues(Set.of(new SubjectPredicateContext(statement.getSubject(), statement.getPredicate(), Collections.emptySet())));

        assertTrue(sut.getAdded().getDefaultModel().listStatements().toList().isEmpty());
    }

    @Test
    void removeStatementsBySubjectAndPredicateRemovesPreviouslyAddedStatementsWithMatchingSubjectPredicateAndContext() {
        final LocalModel sut = new ChangeTrackingLocalModel(true);
        final Statement addedOne = statement(SUBJECT, Vocabulary.RDF_TYPE, TYPE_TWO);
        final Statement addedOther = statement(SUBJECT, Generator.generateUri().toString(), Generator.generateUri().toString());
        sut.addStatements(List.of(addedOne, addedOther), NAMED_GRAPH);
        sut.removePropertyValues(Set.of(new SubjectPredicateContext(addedOne.getSubject(), addedOne.getPredicate(), Set.of(NAMED_GRAPH))));

        assertThat(sut.getAdded().getNamedModel(NAMED_GRAPH).listStatements().toList(), not(hasItem(addedOne)));
        assertThat(sut.getAdded().getNamedModel(NAMED_GRAPH).listStatements().toList(), hasItem(addedOther));
    }
}
