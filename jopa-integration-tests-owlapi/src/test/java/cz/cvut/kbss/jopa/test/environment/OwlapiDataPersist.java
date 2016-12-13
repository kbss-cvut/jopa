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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.Triple;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.util.Collection;

public class OwlapiDataPersist {

    public void persistTestData(Collection<Triple> data, EntityManager em) throws Exception {
        final OWLOntology ontology = em.unwrap(OWLOntology.class);
        final OWLOntologyManager manager = ontology.getOWLOntologyManager();
        final OWLDataFactory df = manager.getOWLDataFactory();
        for (Triple t : data) {
            final OWLNamedIndividual ind = df.getOWLNamedIndividual(IRI.create(t.getSubject()));
            final AddAxiom axiom;
            if (t.getProperty().toString().equals(CommonVocabulary.RDF_TYPE)) {
                final OWLClass cls = df.getOWLClass(IRI.create(t.getValue().toString()));
                axiom = new AddAxiom(ontology, df.getOWLClassAssertionAxiom(cls, ind));
            } else if (t.getValue() instanceof URI) {
                final OWLObjectProperty op = df.getOWLObjectProperty(IRI.create(t.getProperty()));
                final OWLNamedIndividual obj = df.getOWLNamedIndividual(IRI.create((URI) t.getValue()));
                axiom = new AddAxiom(ontology, df.getOWLObjectPropertyAssertionAxiom(op, ind, obj));
            } else if (t.getProperty().toString().equals(CommonVocabulary.RDFS_LABEL)) {
                final OWLAnnotationProperty ap = df.getOWLAnnotationProperty(IRI.create(t.getProperty()));
                final OWLLiteral value = OwlapiUtils.createOWLLiteralFromValue(t.getValue(), df, "en");
                axiom = new AddAxiom(ontology, df.getOWLAnnotationAssertionAxiom(ap, ind.getIRI(), value));
            } else {
                final OWLDataProperty dp = df.getOWLDataProperty(IRI.create(t.getProperty()));
                final OWLLiteral value = OwlapiUtils.createOWLLiteralFromValue(t.getValue(), df, "en");
                axiom = new AddAxiom(ontology, df.getOWLDataPropertyAssertionAxiom(dp, ind, value));
            }
            manager.applyChange(axiom);
        }
    }
}
