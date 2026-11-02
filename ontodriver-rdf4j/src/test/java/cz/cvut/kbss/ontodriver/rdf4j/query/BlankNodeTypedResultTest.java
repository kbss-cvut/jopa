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
package cz.cvut.kbss.ontodriver.rdf4j.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.Arrays;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Regression tests for mapping a SELECT result row whose bound value is a blank node.
 * <p>
 * Before the fix, {@link #getObjectTypedOnBlankNodeReturnsTheBlankNodeLabelAsUri()} hit the RDF4j
 * typed extraction, which only handled {@code Literal} and {@code IRI} values. A blank node matched
 * neither branch, so the value was left {@code null} and surfaced as a
 * {@link NullPointerException} when the result was mapped to a non-null type such as
 * {@code URI.class} (see the issue about a {@code getResultList} throwing a NPE when the query
 * returns blank nodes). The typed extraction now falls back to the blank node's label (its string
 * value) as a relative URI, mirroring what the Jena driver returns for a blank node and staying
 * consistent with the untyped extraction in this class.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class BlankNodeTypedResultTest {

    private static final java.util.List<String> BINDINGS = Arrays.asList("x", "y", "z");

    @Mock
    private TupleQueryResult resultMock;
    @Mock
    private BindingSet bindingSetMock;
    @Mock
    private Statement statementMock;

    private final ValueFactory valueFactory = SimpleValueFactory.getInstance();

    private ResultSet resultSet;

    @BeforeEach
    public void setUp() {
        when(resultMock.getBindingNames()).thenReturn(BINDINGS);
        when(resultMock.next()).thenReturn(bindingSetMock);
        when(resultMock.hasNext()).thenReturn(true);
        this.resultSet = new SelectResultSet(resultMock, statementMock);
    }

    @Test
    public void getObjectTypedOnBlankNodeReturnsTheBlankNodeLabelAsUri() throws Exception {
        final String label = "blankNode495";
        when(bindingSetMock.getValue("z")).thenReturn(valueFactory.createBNode(label));

        resultSet.next();
        final URI result = resultSet.getObject("z", URI.class);
        assertNotNull(result, "a blank node mapped to URI.class must not be null");
        assertEquals(URI.create(label), result);
    }

    @Test
    public void getObjectTypedOnBlankNodeDoesNotReturnNull() throws Exception {
        when(bindingSetMock.getValue("z")).thenReturn(valueFactory.createBNode("someLabel"));

        resultSet.next();
        // This mirrors NonEntityQueryResultLoader.loadResult: Optional.of(resultRow.getObject(0, type)).
        // Before the fix getObject returned null for a blank node, and Optional.of(null) was the NPE.
        final Optional<URI> wrapped = Optional.of(resultSet.getObject("z", URI.class));
        assertTrue(wrapped.isPresent());
    }

    @Test
    public void getObjectOnBlankNodeStillReturnsItsStringForm() throws Exception {
        final String label = "untypedLabel";
        when(bindingSetMock.getValue("z")).thenReturn(valueFactory.createBNode(label));

        resultSet.next();
        // The untyped extraction is unchanged: a blank node is returned as its string form
        // (the value's toString(), i.e. "_:<label>"). Guard against a regression there.
        final Object result = resultSet.getObject("z");
        assertEquals("_:" + label, result);
    }
}
