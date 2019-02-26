/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.util.Collection;

import static org.junit.Assert.assertTrue;

public class OwlapiDataAccessor implements DataAccessor {

    @Override
    public void persistTestData(Collection<Triple> data, EntityManager em) {
        final OWLOntology ontology = em.unwrap(OWLOntology.class);
        final OWLOntologyManager manager = ontology.getOWLOntologyManager();
        final OWLDataFactory df = manager.getOWLDataFactory();
        for (Triple t : data) {
            final OWLNamedIndividual ind = df.getOWLNamedIndividual(IRI.create(t.getSubject()));
            final AddAxiom axiom;
            if (t.getProperty().toString().equals(RDF.TYPE)) {
                final OWLClass cls = df.getOWLClass(IRI.create(t.getValue().toString()));
                axiom = new AddAxiom(ontology, df.getOWLClassAssertionAxiom(cls, ind));
            } else if (t.getProperty().toString().equals(RDFS.SUB_CLASS_OF)) {
                final OWLClass subclass = df.getOWLClass(IRI.create(t.getSubject().toString()));
                final OWLClass superclass = df.getOWLClass(IRI.create(t.getValue().toString()));
                axiom = new AddAxiom(ontology, df.getOWLSubClassOfAxiom(subclass, superclass));
            } else if (t.getValue() instanceof URI) {
                final OWLObjectProperty op = df.getOWLObjectProperty(IRI.create(t.getProperty()));
                final OWLNamedIndividual obj = df.getOWLNamedIndividual(IRI.create((URI) t.getValue()));
                axiom = new AddAxiom(ontology, df.getOWLObjectPropertyAssertionAxiom(op, ind, obj));
            } else if (t.getProperty().toString().equals(RDFS.LABEL)) {
                final OWLAnnotationProperty ap = df.getOWLAnnotationProperty(IRI.create(t.getProperty()));
                final OWLLiteral value = OwlapiUtils.createOWLLiteralFromValue(t.getValue(), df, t.getLanguage());
                axiom = new AddAxiom(ontology, df.getOWLAnnotationAssertionAxiom(ap, ind.getIRI(), value));
            } else {
                final OWLDataProperty dp = df.getOWLDataProperty(IRI.create(t.getProperty()));
                final OWLLiteral value = OwlapiUtils.createOWLLiteralFromValue(t.getValue(), df, t.getLanguage());
                axiom = new AddAxiom(ontology, df.getOWLDataPropertyAssertionAxiom(dp, ind, value));
            }
            manager.applyChange(axiom);
        }
    }

    @Override
    public void verifyDataPresence(Collection<Triple> data, EntityManager em) {
        final OWLOntology ontology = em.unwrap(OWLOntology.class);
        final OWLDataFactory df = ontology.getOWLOntologyManager().getOWLDataFactory();
        for (Triple t : data) {
            final OWLNamedIndividual ind = df.getOWLNamedIndividual(IRI.create(t.getSubject()));
            if (t.getProperty().toString().equals(RDF.TYPE)) {
                final OWLClass cls = df.getOWLClass(IRI.create(t.getValue().toString()));
                assertTrue(ontology.containsAxiom(df.getOWLClassAssertionAxiom(cls, ind)));
            } else if (t.getValue() instanceof URI) {
                final OWLObjectProperty op = df.getOWLObjectProperty(IRI.create(t.getProperty()));
                final OWLNamedIndividual obj = df.getOWLNamedIndividual(IRI.create((URI) t.getValue()));
                assertTrue(ontology.containsAxiom(df.getOWLObjectPropertyAssertionAxiom(op, ind, obj)));
            } else {
                final OWLAnnotationProperty ap = df.getOWLAnnotationProperty(IRI.create(t.getProperty()));
                final OWLLiteral value = OwlapiUtils.createOWLLiteralFromValue(t.getValue(), df, t.getLanguage());
                final OWLAxiom apAxiom = df.getOWLAnnotationAssertionAxiom(ap, ind.getIRI(), value);
                final OWLDataProperty dp = df.getOWLDataProperty(IRI.create(t.getProperty()));
                final OWLAxiom dpAxiom = df.getOWLDataPropertyAssertionAxiom(dp, ind, value);
                assertTrue(ontology.containsAxiom(apAxiom) || ontology.containsAxiom(dpAxiom));
            }
        }
    }
}
