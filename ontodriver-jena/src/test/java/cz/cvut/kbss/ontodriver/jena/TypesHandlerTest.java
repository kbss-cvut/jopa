/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static cz.cvut.kbss.ontodriver.util.Vocabulary.RDF_TYPE;
import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class TypesHandlerTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RESOURCE = createResource(SUBJECT.getIdentifier().toString());

    private Set<String> types;

    @Mock
    private StorageConnector connectorMock;

    @Mock
    private InferredStorageConnector inferredConnectorMock;

    private TypesHandler handler;

    @BeforeEach
    public void setUp() {
        this.handler = new TypesHandler(connectorMock, inferredConnectorMock);
        this.types = generateTypes();
    }

    private static Set<String> generateTypes() {
        return IntStream.range(0, 5).mapToObj(i -> Generator.generateUri().toString()).collect(Collectors.toSet());
    }

    @Test
    public void getTypesLoadsTypesFromStorage() {
        final Set<Statement> statements = generateTypesStatements();
        final Set<Axiom<URI>> result = handler.getTypes(SUBJECT, Collections.emptySet(), false);
        verifyLoadedTypes(statements, result, false);
        verify(inferredConnectorMock, never()).findWithInference(any(), any(), any(), anyCollection());
        verify(connectorMock).find(SUBJECT_RESOURCE, RDF.type, null, Collections.emptySet());
    }

    private Set<Statement> generateTypesStatements() {
        final Set<Statement> statements = statementsForTypes();
        when(connectorMock.find(any(), any(), any(), any())).thenReturn(statements);
        return statements;
    }

    private Set<Statement> statementsForTypes() {
        return types.stream().map(t -> ResourceFactory
                .createStatement(SUBJECT_RESOURCE, RDF.type, createResource(t)))
                    .collect(Collectors.toSet());
    }

    private void verifyLoadedTypes(Set<Statement> statements, Set<Axiom<URI>> result, boolean inferred) {
        assertEquals(result.size(), statements.size());
        result.forEach(a -> {
            assertEquals(SUBJECT, a.getSubject());
            assertEquals(Assertion.createClassAssertion(inferred), a.getAssertion());
            assertTrue(types.contains(a.getValue().stringValue()));
        });
    }

    @Test
    public void getTypesLoadsTypesFromContext() {
        final URI context = Generator.generateUri();
        final Set<Statement> statements = generateTypesStatements();
        final Set<Axiom<URI>> result = handler.getTypes(SUBJECT, Collections.singleton(context), false);
        verifyLoadedTypes(statements, result, false);
        verify(connectorMock).find(SUBJECT_RESOURCE, RDF.type, null, Collections.singleton(context.toString()));
    }

    @Test
    public void addTypesInsertsStatementsIntoStorage() {
        final Set<String> types = generateTypes();
        handler.addTypes(SUBJECT, null, types.stream().map(URI::create).collect(Collectors.toSet()));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(null));
        verifyTypesStatements(types, captor.getValue());
    }

    private void verifyTypesStatements(Set<String> types, List expected) {
        expected.forEach(item -> {
            final Statement statement = (Statement) item;
            assertEquals(SUBJECT_RESOURCE, statement.getSubject());
            assertEquals(createProperty(RDF_TYPE), statement.getPredicate());
            assertTrue(types.contains(statement.getObject().asResource().getURI()));
        });
    }

    @Test
    public void addTypesInsertsStatementsIntoStorageContext() {
        final Set<String> types = generateTypes();
        final URI context = Generator.generateUri();
        handler.addTypes(SUBJECT, context, types.stream().map(URI::create).collect(Collectors.toSet()));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).add(captor.capture(), eq(context.toString()));
        verifyTypesStatements(types, captor.getValue());
    }

    @Test
    public void removeTypesRemovesStatementsFromStorage() {
        final Set<String> types = generateTypes();
        handler.removeTypes(SUBJECT, null, types.stream().map(URI::create).collect(Collectors.toSet()));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).remove(captor.capture(), eq(null));
        verifyTypesStatements(types, captor.getValue());
    }

    @Test
    public void removeTypesRemovesStatementsFromStorageContext() {
        final Set<String> types = generateTypes();
        final URI context = Generator.generateUri();
        handler.removeTypes(SUBJECT, context, types.stream().map(URI::create).collect(Collectors.toSet()));
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(connectorMock).remove(captor.capture(), eq(context.toString()));
        verifyTypesStatements(types, captor.getValue());
    }

    @Test
    public void getTypesLoadsTypesWithInferenceWhenInferredSwitchIsOn() {
        final Set<Statement> statements = statementsForTypes();
        when(inferredConnectorMock.findWithInference(any(), any(), any(), any())).thenReturn(statements);
        final Set<Axiom<URI>> result = handler.getTypes(SUBJECT, Collections.emptySet(), true);
        verifyLoadedTypes(statements, result, true);
        verify(connectorMock, never()).find(SUBJECT_RESOURCE, RDF.type, null, Collections.emptySet());
        verify(inferredConnectorMock).findWithInference(SUBJECT_RESOURCE, RDF.type, null, Collections.emptySet());
    }
}
