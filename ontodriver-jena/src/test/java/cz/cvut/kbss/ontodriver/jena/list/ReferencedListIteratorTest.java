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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.exception.ListProcessingException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.apache.jena.rdf.model.ResourceFactory.createLangLiteral;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.apache.jena.rdf.model.ResourceFactory.createTypedLiteral;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ReferencedListIteratorTest extends ListIteratorTestBase<ReferencedListIterator<NamedResource>> {

    @BeforeEach
    public void setUp() {
        super.setUp();
        testUtil.setHasContent(HAS_CONTENT);
    }

    @Override
    ReferencedListIterator<NamedResource> iterator() {
        return new ReferencedListIterator<>(descriptor(Assertion.AssertionType.OBJECT_PROPERTY, null), connectorMock);
    }

    ReferencedListDescriptor descriptor(Assertion.AssertionType contentAssertionType, String context) {
        final NamedResource owner = NamedResource.create(RESOURCE.getURI());
        final Assertion hasList = Assertion.createObjectPropertyAssertion(URI.create(HAS_LIST.getURI()), false);
        final Assertion hasNext = Assertion.createObjectPropertyAssertion(URI.create(HAS_NEXT.getURI()), false);
        final Assertion hasContent;
        if (contentAssertionType == Assertion.AssertionType.OBJECT_PROPERTY) {
            hasContent = Assertion.createObjectPropertyAssertion(URI.create(HAS_CONTENT.getURI()), false);
        } else {
            hasContent = Assertion.createDataPropertyAssertion(URI.create(HAS_CONTENT.getURI()), false);
        }
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(owner, hasList, hasNext,
                hasContent);
        if (context != null) {
            descriptor.setContext(URI.create(context));
        }
        return descriptor;
    }

    @Override
    List<URI> generateList() {
        return testUtil.generateReferencedList();
    }

    @Test
    public void nextReturnsFirstListElement() {
        final List<URI> list = generateList();
        final ReferencedListIterator<NamedResource> iterator = iterator();
        assertTrue(iterator.hasNext());
        final Axiom<NamedResource> head = iterator.nextAxiom();
        assertNotNull(head);
        assertEquals(descriptor(Assertion.AssertionType.OBJECT_PROPERTY, null).getNodeContent(), head.getAssertion());
        assertEquals(NamedResource.create(list.get(0)), head.getValue().getValue());
    }

    @Test
    public void nextThrowsListProcessingExceptionWhenNodeIsLiteral() {
        generateList();
        when(connectorMock.find(RESOURCE, HAS_LIST, null, Collections.emptySet())).thenReturn(
                Collections.singletonList(createStatement(RESOURCE, HAS_LIST, createTypedLiteral(117))));
        final ReferencedListIterator<NamedResource> iterator = iterator();
        final ListProcessingException ex = assertThrows(ListProcessingException.class, () -> {
            while (iterator.hasNext()) {
                iterator.nextAxiom();
            }
        });
        assertThat(ex.getMessage(),
                containsString("Expected successor of node " + RESOURCE + " to be a named resource."));
    }

    @Test
    public void nextThrowsIntegrityConstraintViolationExceptionWhenNodeHasMultipleContentValues() {
        final List<URI> list = generateList();
        final int index = Generator.randomInt(list.size());
        final Resource node = testUtil.getReferencedListNodes().get(index);
        when(connectorMock.find(node, HAS_CONTENT, null, Collections.emptySet())).thenReturn(Arrays.asList(
                createStatement(node, HAS_CONTENT, createResource(Generator.generateUri().toString())),
                createStatement(node, HAS_CONTENT, createResource(list.get(index).toString()))
        ));
        final ReferencedListIterator<NamedResource> iterator = iterator();
        final IntegrityConstraintViolatedException ex = assertThrows(IntegrityConstraintViolatedException.class, () -> {
            while (iterator.hasNext()) {
                iterator.nextAxiom();
            }
        });
        assertThat(ex.getMessage(),
                containsString("Expected exactly one content statement for node <" + node.getURI() + ">"));
    }

    @Test
    public void nextThrowsIntegrityConstraintViolationExceptionWhenNodeHasNoContent() {
        final List<URI> list = generateList();
        final int index = Generator.randomInt(list.size());
        final Resource node = testUtil.getReferencedListNodes().get(index);
        when(connectorMock.find(node, HAS_CONTENT, null, Collections.emptySet())).thenReturn(Collections.emptyList());
        final ReferencedListIterator<NamedResource> iterator = iterator();
        final IntegrityConstraintViolatedException ex = assertThrows(IntegrityConstraintViolatedException.class, () -> {
            while (iterator.hasNext()) {
                iterator.nextAxiom();
            }
        });
        assertThat(ex.getMessage(), containsString("Expected exactly one content statement for node <" + node.getURI() + ">"));
    }

    @Test
    public void removeWithoutReconnectRemovesLastElementInList() {
        generateList();
        final ReferencedListIterator<NamedResource> iterator = iterator();
        while (iterator.hasNext()) {
            iterator.nextAxiom();
        }
        iterator.removeWithoutReconnect();
        final List<Resource> nodes = testUtil.getReferencedListNodes();
        verify(connectorMock).remove(nodes.get(nodes.size() - 2), HAS_NEXT, nodes.get(nodes.size() - 1), null);
        verify(connectorMock).remove(eq(nodes.get(nodes.size() - 1)), eq(HAS_CONTENT), any(), eq(null));
    }

    @Test
    public void removeWithoutReconnectRemovesFirstElementInList() {
        generateList();
        final ReferencedListIterator<NamedResource> iterator = iterator();
        iterator.nextValue();
        iterator.removeWithoutReconnect();
        final List<Resource> nodes = testUtil.getReferencedListNodes();
        verify(connectorMock).remove(RESOURCE, HAS_LIST, nodes.get(0), null);
        verify(connectorMock).remove(eq(nodes.get(0)), eq(HAS_CONTENT), any(), eq(null));
    }

    @Test
    public void replaceReplacesNodeContent() {
        generateList();
        final ReferencedListIterator<NamedResource> iterator = iterator();
        iterator.nextValue();
        final NamedResource replacement = NamedResource.create(Generator.generateUri());
        iterator.replace(replacement);

        final List<Resource> nodes = testUtil.getReferencedListNodes();
        verify(connectorMock).remove(nodes.get(0), HAS_CONTENT, null, null);
        final Statement added = createStatement(nodes.get(0), HAS_CONTENT, createResource(replacement.getIdentifier()
                                                                                                     .toString()));
        verify(connectorMock).add(Collections.singletonList(added), null);
    }

    @Test
    public void replaceReplacesNodeContentInContext() {
        final String context = Generator.generateUri().toString();
        testUtil.generateReferencedList(context);
        final ReferencedListIterator<NamedResource> iterator = new ReferencedListIterator<>(descriptor(Assertion.AssertionType.OBJECT_PROPERTY, context), connectorMock);
        iterator.nextValue();
        final NamedResource replacement = NamedResource.create(Generator.generateUri());
        iterator.replace(replacement);

        final List<Resource> nodes = testUtil.getReferencedListNodes();
        verify(connectorMock).remove(nodes.get(0), HAS_CONTENT, null, context);
        final Statement added = createStatement(nodes.get(0), HAS_CONTENT, createResource(replacement.getIdentifier()
                                                                                                     .toString()));
        verify(connectorMock).add(Collections.singletonList(added), context);
    }

    @Test
    void nextAxiomReturnsAxiomWithMultilingualStringValueWhenNodeContentIsMultilingualString() {
        final Resource node = createResource(Generator.generateUri().toString());
        final Statement next = createStatement(RESOURCE, HAS_LIST, node);
        when(connectorMock.find(RESOURCE, HAS_LIST, null, Collections.emptySet())).thenReturn(List.of(next));
        final List<Statement> content = List.of(
                createStatement(node, HAS_CONTENT, createLangLiteral("one", "en")),
                createStatement(node, HAS_CONTENT, createLangLiteral("jedna", "cs"))
        );
        when(connectorMock.find(node, HAS_CONTENT, null, Collections.emptySet())).thenReturn(content);
        final ReferencedListIterator<Translations> sut = new ReferencedListIterator<>(descriptor(Assertion.AssertionType.DATA_PROPERTY, null), connectorMock);

        final Axiom<Translations> result = sut.nextAxiom();
        assertNotNull(result);
        assertEquals(new Translations(Map.of("en", "one", "cs", "jedna")), result.getValue().getValue());
    }

    @Test
    void replaceReplacesAllTranslationsOfMultilingualStringInCurrentNode() {
        final Resource node = createResource(Generator.generateUri().toString());
        final Statement next = createStatement(RESOURCE, HAS_LIST, node);
        when(connectorMock.find(RESOURCE, HAS_LIST, null, Collections.emptySet())).thenReturn(List.of(next));
        final List<Statement> content = List.of(
                createStatement(node, HAS_CONTENT, createLangLiteral("one", "en")),
                createStatement(node, HAS_CONTENT, createLangLiteral("jedna", "cs"))
        );
        when(connectorMock.find(node, HAS_CONTENT, null, Collections.emptySet())).thenReturn(content);
        final ReferencedListIterator<Translations> sut = new ReferencedListIterator<>(descriptor(Assertion.AssertionType.DATA_PROPERTY, null), connectorMock);

        final Translations newContent = new Translations(Map.of(
                "en", "New",
                "cs", "Nový"
        ));
        sut.nextAxiom();
        sut.replace(newContent);
        verify(connectorMock).remove(node, HAS_CONTENT, null, null);
        final ArgumentCaptor<List<Statement>> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), isNull());
        assertEquals(newContent.getValue().size(), captor.getValue().size());
        captor.getValue().forEach(s -> {
            assertTrue(s.getObject().isLiteral());
            assertEquals(newContent.get(s.getObject().asLiteral().getLanguage()), s.getObject().asLiteral().getValue());
        });
    }
}
