/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.SubjectPredicateContext;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
public class EpistemicAxiomRemoverTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RESOURCE = ResourceFactory.createResource(SUBJECT.getIdentifier().toString());

    @Mock
    private StorageConnector connectorMock;

    private EpistemicAxiomRemover remover;

    @BeforeEach
    public void setUp() {
        this.remover = new EpistemicAxiomRemover(connectorMock);
    }

    @Test
    public void removeRemovesStatementsByProperty() {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        final Assertion dp = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(dp);
        final Assertion op = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(op);

        remover.remove(descriptor);
        verify(connectorMock).removePropertyValues(Set.of(
                new SubjectPredicateContext(SUBJECT_RESOURCE, createProperty(dp.getIdentifier().toString()), Collections.emptySet()),
                new SubjectPredicateContext(SUBJECT_RESOURCE, createProperty(op.getIdentifier().toString()), Collections.emptySet())
        ));
    }

    @Test
    public void removeRemovesStatementsByPropertyFromCorrectContexts() {
        final URI mainContext = Generator.generateUri();
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addSubjectContext(mainContext);
        final Assertion ca = Assertion.createClassAssertion(false);
        descriptor.addAssertion(ca);
        final Assertion ap = Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), false);
        descriptor.addAssertion(ap);
        final URI assertionContext = Generator.generateUri();
        descriptor.addAssertionContext(ap, assertionContext);

        remover.remove(descriptor);
        verify(connectorMock).removePropertyValues(Set.of(
                new SubjectPredicateContext(SUBJECT_RESOURCE, createProperty(ca.getIdentifier().toString()), Set.of(mainContext.toString())),
                new SubjectPredicateContext(SUBJECT_RESOURCE, createProperty(ap.getIdentifier().toString()), Set.of(assertionContext.toString()))
        ));
    }
}
