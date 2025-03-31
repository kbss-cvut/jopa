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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.SubjectPredicateContext;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
public class EpistemicAxiomRemoverTest {

    private static final NamedResource SUBJECT = NamedResource
            .create("https://onto.fel.cvut.cz/ontologies/jopa/entityX");

    private static final String PROPERTY = "https://onto.fel.cvut.cz/ontologies/jopa/propertyOne";

    private AxiomDescriptor descriptor;

    @Mock
    private RepoConnection connectorMock;

    private ValueFactory vf;

    private EpistemicAxiomRemover axiomRemover;

    @BeforeEach
    public void setUp() {
        this.vf = SimpleValueFactory.getInstance();
        this.descriptor = new AxiomDescriptor(SUBJECT);

        this.axiomRemover = new EpistemicAxiomRemover(connectorMock, vf);
    }

    @Test
    public void removeWithAssertionContextSpecifiesContextForSubjectAndPredicateRemoval() throws Exception {
        final URI context = Generator.generateUri();
        final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(PROPERTY), false);
        descriptor.addAssertion(ass);
        descriptor.addAssertionContext(ass, context);

        axiomRemover.remove(descriptor);

        verify(connectorMock).removePropertyValues(Set.of(new SubjectPredicateContext(vf.createIRI(SUBJECT.toString()),
                                                                                      vf.createIRI(PROPERTY), Set.of(vf.createIRI(context.toString())))));
    }

    @Test
    public void removeWithoutAssertionContextUsesEmptyContextCollectionForSubjectAndPredicateRemoval() throws Exception {
        final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(PROPERTY), false);
        descriptor.addAssertion(ass);

        axiomRemover.remove(descriptor);

        verify(connectorMock).removePropertyValues(Set.of(new SubjectPredicateContext(vf.createIRI(SUBJECT.toString()),
                                                                                      vf.createIRI(PROPERTY), Collections.emptySet())));
    }
}
