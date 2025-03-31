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
package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ReferencedListIteratorTest {

    private static final ValueFactory VF = SimpleValueFactory.getInstance();

    @Mock
    private RepoConnection connector;

    @Test
    void nextNodeThrowsIntegrityConstraintViolatedExceptionWhenNodeHasMultipleContentValues() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createIRI(Generator.generateUri()
                                                                                                                      .toString())),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createIRI(Generator.generateUri()
                                                                                                                      .toString())))
        );
        final ReferencedListIterator<NamedResource> sut = new ReferencedListIterator<>(descriptor(ReferencedListHandlerTest.nodeContentAssertion), connector, VF);

        assertThrows(IntegrityConstraintViolatedException.class, sut::nextNode);
    }

    private static ReferencedListDescriptor descriptor(Assertion hasContent) {
        return new ReferencedListDescriptorImpl(ListHandlerTestHelper.OWNER, ReferencedListHandlerTest.hasListAssertion,
                ReferencedListHandlerTest.nextNodeAssertion, hasContent);
    }

    @Test
    void nextNodeAllowsMultipleTranslationsOfStringAsNodeContentValues() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("One", "en")),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("Jedna", "cs")))
        );
        final ReferencedListIterator<Translations> sut = new ReferencedListIterator<>(
                descriptor(Assertion.createDataPropertyAssertion(URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false)),
                connector, VF);

        assertDoesNotThrow(sut::nextNode);
    }

    @Test
    void nextAxiomReturnsAxiomWithMultilingualStringWhenNodeContentIsMultilingualString() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("One", "en")),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("Jedna", "cs")))
        );
        final ReferencedListIterator<Translations> sut = new ReferencedListIterator<>(
                descriptor(Assertion.createDataPropertyAssertion(URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false)),
                connector, VF);

        final Axiom<Translations> result = sut.nextAxiom();
        assertNotNull(result);
        assertEquals(new Translations(Map.of("en", "One", "cs", "Jedna")), result.getValue().getValue());
    }

    @Test
    void currentContentReturnsMultilingualStringWhenNodeContentIsMultilingualString() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("One", "en")),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("Jedna", "cs")))
        );
        final ReferencedListIterator<Translations> sut = new ReferencedListIterator<>(
                descriptor(Assertion.createDataPropertyAssertion(URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false)),
                connector, VF);

        sut.nextNode();
        final Translations result = sut.currentContent();
        assertEquals(new Translations(Map.of("en", "One", "cs", "Jedna")), result);
    }

    @Test
    void replaceCurrentWithReplacesMultilingualStringStatementsWithProvidedValues() throws Exception {
        final IRI node = VF.createIRI(Generator.generateUri().toString());
        final Statement nextStatement = VF.createStatement(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, node);
        when(connector.findStatements(ReferencedListHandlerTest.owner, ReferencedListHandlerTest.hasListProperty, null, false, Collections.emptySet())).thenReturn(List.of(nextStatement));
        when(connector.findStatements(node, ReferencedListHandlerTest.nodeContentProperty, null, false, Collections.emptySet())).thenReturn(
                List.of(VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("One", "en")),
                        VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("Jedna", "cs")))
        );
        final ReferencedListIterator<Translations> sut = new ReferencedListIterator<>(
                descriptor(Assertion.createDataPropertyAssertion(URI.create(ListHandlerTestHelper.NODE_CONTENT_PROPERTY), false)),
                connector, VF);

        final Translations newValue = new Translations(Map.of("en", "new one", "cs", "nová jedna"));
        sut.nextNode();
        sut.replaceCurrentWith(newValue);
        verify(connector).removeStatements(List.of(
                VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("One", "en")),
                VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral("Jedna", "cs"))
        ));
        verify(connector).addStatements(newValue.getValue().entrySet().stream()
                                                .map(e -> VF.createStatement(node, ReferencedListHandlerTest.nodeContentProperty, VF.createLiteral(e.getValue(), e.getKey())))
                                                .collect(Collectors.toList()));
    }
}
