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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableRemoveAxiom;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNode;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class TypesHandlerTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#Entity");
    private static final NamedResource INDIVIDUAL = NamedResource.create(PK);

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private OWLOntology ontologyMock;
    @Mock
    private OWLOntologyManager managerMock;
    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory dataFactory;

    private TypesHandler typesHandler;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        this.dataFactory = new OWLDataFactoryImpl();
        final OntologySnapshot snapshot = new OntologySnapshot(ontologyMock, managerMock, dataFactory,
                reasonerMock);
        this.typesHandler = new TypesHandler(adapterMock, snapshot);
    }

    @Test
    void getTypesLoadsExplicitTypesFromOntology() throws Exception {
        final OntologySnapshot snapshot = TestUtils.initRealOntology(reasonerMock);
        final Set<URI> types = initTypes();
        addClassAssertionsToOntology(types, snapshot.getOntology(), snapshot.getOntologyManager());
        final Set<Axiom<URI>> result = new TypesHandler(adapterMock, snapshot)
                .getTypes(INDIVIDUAL, Collections.emptySet(), false);

        assertEquals(types.size(), result.size());
        for (Axiom<URI> ax : result) {
            assertTrue(types.contains(ax.getValue().getValue()));
        }
    }

    private void addClassAssertionsToOntology(Collection<URI> classes, OWLOntology ontology,
                                              OWLOntologyManager ontologyManager) {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(TypesHandlerTest.PK));
        classes.forEach(uri -> {
            final OWLClass cls = dataFactory.getOWLClass(IRI.create(uri));
            final OWLClassAssertionAxiom a = dataFactory.getOWLClassAssertionAxiom(cls, individual);
            ontologyManager.applyChange(new AddAxiom(ontology, a));
        });
    }

    private Set<URI> initTypes() {
        final Set<URI> types = new HashSet<>();
        for (int i = 0; i < 5; i++) {
            types.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#OwlapiType" + i));
        }
        return types;
    }

    @Test
    void getTypesLoadsTypesIncludingInferred() {
        final Set<URI> typeUris = initTypes();
        final NodeSet<OWLClass> types = new OWLClassNodeSet(typeUris.stream().map(
                uri -> new OWLClassNode(dataFactory.getOWLClass(IRI.create(uri)))).collect(
                Collectors.toSet()));
        when(reasonerMock.getTypes(dataFactory.getOWLNamedIndividual(IRI.create(PK)), false)).thenReturn(types);

        final Set<Axiom<URI>> result = typesHandler.getTypes(INDIVIDUAL, null, true);

        assertEquals(typeUris.size(), result.size());
        for (Axiom<URI> ax : result) {
            assertTrue(typeUris.contains(ax.getValue().getValue()));
        }
    }

    @Test
    void addTypesAddsClassAssertionAxiomsForTheTypes() {
        final Set<URI> typeUris = initTypes();

        typesHandler.addTypes(INDIVIDUAL, null, typeUris);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(managerMock).applyChanges(captor.capture());
        final List<?> addChanges = captor.getValue();
        assertEquals(typeUris.size(), addChanges.size());
        for (Object change : addChanges) {
            assertTrue(change instanceof MutableAddAxiom);
            final MutableAddAxiom ax = (MutableAddAxiom) change;
            final OWLClassAssertionAxiom clsAxiom = (OWLClassAssertionAxiom) ax.getAxiom();
            assertTrue(typeUris.contains(clsAxiom.getClassExpression().asOWLClass().getIRI().toURI()));
        }
        verify(adapterMock).addTransactionalChanges(anyList());
    }

    @Test
    void removeTypesRemovesClassAssertionsAboutTheTypes() {
        final Set<URI> typeUris = initTypes();

        typesHandler.removeTypes(INDIVIDUAL, null, typeUris);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(managerMock).applyChanges(captor.capture());
        final List<?> removeChanges = captor.getValue();
        assertEquals(typeUris.size(), removeChanges.size());
        for (Object change : removeChanges) {
            assertTrue(change instanceof MutableRemoveAxiom);
            final MutableRemoveAxiom ax = (MutableRemoveAxiom) change;
            final OWLClassAssertionAxiom clsAxiom = (OWLClassAssertionAxiom) ax.getAxiom();
            assertTrue(typeUris.contains(clsAxiom.getClassExpression().asOWLClass().getIRI().toURI()));
        }
        verify(adapterMock).addTransactionalChanges(anyList());
    }

    @Test
    void getTypesLoadsTypesFromImportedOntologiesAsWell() throws Exception {
        final OntologySnapshot snapshot = TestUtils.initRealOntology(reasonerMock);
        final Set<URI> types = initTypes();
        final OWLOntologyManager manager = snapshot.getOntologyManager();
        final OWLOntology imported = manager.createOntology(IRI.create(Generator.generateUri()));
        manager.applyChange(new AddImport(snapshot.getOntology(),
                dataFactory.getOWLImportsDeclaration(imported.getOntologyID().getOntologyIRI().get())));
        addClassAssertionsToOntology(types, snapshot.getOntology(), snapshot.getOntologyManager());
        final URI typeInImport = Generator.generateUri();
        addClassAssertionsToOntology(Collections.singleton(typeInImport), imported, manager);
        final Set<Axiom<URI>> result = new TypesHandler(adapterMock, snapshot).getTypes(INDIVIDUAL, null, false);

        final Set<URI> resultTypes = result.stream().map(a -> a.getValue().getValue()).collect(Collectors.toSet());
        assertTrue(resultTypes.containsAll(types));
        assertTrue(resultTypes.contains(typeInImport));
    }
}
