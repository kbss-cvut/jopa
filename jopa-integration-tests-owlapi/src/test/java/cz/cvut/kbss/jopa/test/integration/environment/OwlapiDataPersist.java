package cz.cvut.kbss.jopa.test.integration.environment;

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
            if (t.getValue() instanceof URI) {
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
