/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
import java.util.Collection;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class OwlapiDataAccessor implements DataAccessor {

    @Override
    public void persistTestData(Collection<Quad> data, EntityManager em) {
        final OWLOntology ontology = em.unwrap(OWLOntology.class);
        final OWLOntologyManager manager = ontology.getOWLOntologyManager();
        final OWLDataFactory df = manager.getOWLDataFactory();
        for (Quad t : data) {
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
    public void verifyDataPresent(Collection<Quad> data, EntityManager em) {
        final OWLOntology ontology = em.unwrap(OWLOntology.class);
        data.forEach(t -> assertTrue(doesAxiomExist(t, ontology)));
    }

    private boolean doesAxiomExist(Quad quad, OWLOntology ontology) {
        final OWLDataFactory df = ontology.getOWLOntologyManager().getOWLDataFactory();
        final OWLNamedIndividual ind = df.getOWLNamedIndividual(IRI.create(quad.getSubject()));
        if (quad.getProperty().toString().equals(RDF.TYPE)) {
            final OWLClass cls = df.getOWLClass(IRI.create(quad.getValue().toString()));
            return ontology.containsAxiom(df.getOWLClassAssertionAxiom(cls, ind));
        } else if (quad.getValue() instanceof URI) {
            final OWLObjectProperty op = df.getOWLObjectProperty(IRI.create(quad.getProperty()));
            final OWLNamedIndividual obj = df.getOWLNamedIndividual(IRI.create((URI) quad.getValue()));
            return ontology.containsAxiom(df.getOWLObjectPropertyAssertionAxiom(op, ind, obj));
        } else {
            final OWLAnnotationProperty ap = df.getOWLAnnotationProperty(IRI.create(quad.getProperty()));
            final OWLLiteral value = OwlapiUtils.createOWLLiteralFromValue(quad.getValue(), df, quad.getLanguage());
            final OWLAxiom apAxiom = df.getOWLAnnotationAssertionAxiom(ap, ind.getIRI(), value);
            final OWLDataProperty dp = df.getOWLDataProperty(IRI.create(quad.getProperty()));
            final OWLAxiom dpAxiom = df.getOWLDataPropertyAssertionAxiom(dp, ind, value);
            return ontology.containsAxiom(apAxiom) || ontology.containsAxiom(dpAxiom);
        }
    }

    @Override
    public void verifyDataNotPresent(Collection<Quad> data, EntityManager em) {
        final OWLOntology ontology = em.unwrap(OWLOntology.class);
        data.forEach(t -> assertFalse(doesAxiomExist(t, ontology)));
    }

    public void verifyValueDatatype(OWLOntology ontology, URI identifier, String property, String expectedDatatype) {
        // OWL2Query does not support ASK with a FILTER, so we have to do the check using OWLAPI
        final OWLDataFactory df = ontology.getOWLOntologyManager().getOWLDataFactory();
        EntitySearcher
                .getDataPropertyValues(df.getOWLNamedIndividual(identifier.toString()), df.getOWLDataProperty(property),
                        ontology)
                .forEach(lit -> assertEquals(df.getOWLDatatype(expectedDatatype), lit.getDatatype()));
    }
}
