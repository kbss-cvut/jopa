/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Before;
import org.junit.Test;
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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyList;
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

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.dataFactory = new OWLDataFactoryImpl();
        final OntologySnapshot snapshot = new OntologySnapshot(ontologyMock, managerMock, dataFactory,
                reasonerMock);
        this.typesHandler = new TypesHandler(adapterMock, snapshot);
    }

    @Test
    public void getTypesLoadsExplicitTypesFromOntology() throws Exception {
        final OntologySnapshot snapshot = TestUtils.initRealOntology(reasonerMock);
        final Set<URI> types = initTypes();
        addClassAssertionsToOntology(PK, types, snapshot);
        final Set<Axiom<URI>> result = new TypesHandler(adapterMock, snapshot).getTypes(INDIVIDUAL, null, false);

        assertEquals(types.size(), result.size());
        for (Axiom<URI> ax : result) {
            assertTrue(types.contains(ax.getValue().getValue()));
        }
    }

    private void addClassAssertionsToOntology(URI subject, Collection<URI> classes, OntologySnapshot snapshot) {
        final OWLNamedIndividual individual = snapshot.getDataFactory().getOWLNamedIndividual(IRI.create(subject));
        classes.stream().forEach(uri -> {
            final OWLClass cls = snapshot.getDataFactory().getOWLClass(IRI.create(uri));
            final OWLClassAssertionAxiom a = snapshot.getDataFactory().getOWLClassAssertionAxiom(cls, individual);
            snapshot.getOntologyManager().applyChange(new AddAxiom(snapshot.getOntology(), a));
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
    public void getTypesLoadsTypesIncludingInferred() throws Exception {
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
    public void addTypesAddsClassAssertionAxiomsForTheTypes() throws Exception {
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
    public void removeTypesRemovesClassAssertionsAboutTheTypes() throws Exception {
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
}
