package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.InvalidOntologyIriException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.OntoDriverProperties;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.net.URI;
import java.util.*;

/**
 * Adapter between OntoDriver API and OWLAPI.
 *
 * @author ledvima1
 */
class OwlapiAdapter {

    private final Connector connector;
    private OntologyStructures ontologySnapshot;
    private final String language;

    private TransactionState transactionState = TransactionState.INITIAL;
    private List<OWLOntologyChange> pendingChanges = new ArrayList<>();

    private static enum TransactionState {
        INITIAL, RUNNING
    }

    public OwlapiAdapter(Connector connector, Map<String, String> properties) {
        this.connector = connector;
        this.language = properties.get(OntoDriverProperties.ONTOLOGY_LANGUAGE);
    }

    private void startTransactionIfNotActive() {
        if (transactionState == TransactionState.INITIAL) {
            this.ontologySnapshot = connector.getOntologySnapshot();
            this.transactionState = TransactionState.RUNNING;
        }
    }

    void commit() {
        if (transactionState != TransactionState.RUNNING) {
            return;
        }
        if (!pendingChanges.isEmpty()) {
            connector.applyChanges(pendingChanges);
            this.pendingChanges = new ArrayList<>();
        }
        transactionCleanup();
    }

    private void transactionCleanup() {
        this.ontologySnapshot = null;
        this.transactionState = TransactionState.INITIAL;
    }

    void rollback() {
        if (transactionState != TransactionState.RUNNING) {
            return;
        }
        if (!pendingChanges.isEmpty()) {
            pendingChanges = new ArrayList<>();
        }
        transactionCleanup();
    }

    boolean isConsistent(URI context) {
        startTransactionIfNotActive();
        if (!isContextValid(context)) {
            throw new InvalidOntologyIriException(
                    "Invalid URI passed to isConsistent. Expected " + getOntologyIri() + ", but got " + context);
        }
        return ontologySnapshot.getReasoner().isConsistent();
    }

    private OWLReasoner reasoner() {
        return ontologySnapshot.getReasoner();
    }

    private OWLOntology ontology() {
        return ontologySnapshot.getOntology();
    }

    private OWLDataFactory dataFactory() {
        return ontologySnapshot.getDataFactory();
    }

    private boolean isContextValid(URI context) {
        return context == null || IRI.create(context).equals(getOntologyIri());
    }

    private IRI getOntologyIri() {
        assert ontologySnapshot != null;
        assert ontology().getOntologyID().getOntologyIRI().isPresent();

        return ontology().getOntologyID().getOntologyIRI().get();
    }

    List<URI> getContexts() {
        startTransactionIfNotActive();
        return Collections.singletonList(getOntologyIri().toURI());
    }

    boolean containsAxiom(Axiom<?> axiom, URI context) {
        startTransactionIfNotActive();
        if (!isContextValid(context)) {
            return false;
        }
        final OWLAxiom owlAxiom = getOwlAxiom(axiom);
        if (axiom.getAssertion().isInferred()) {
            return reasoner().isEntailed(owlAxiom);
        } else {
            return ontology().containsAxiom(owlAxiom);
        }
    }

    private OWLAxiom getOwlAxiom(Axiom<?> axiom) {
        final OWLNamedIndividual individual = dataFactory().getOWLNamedIndividual(
                IRI.create(axiom.getSubject().getIdentifier()));
        OWLAxiom owlAxiom = null;
        switch (axiom.getAssertion().getType()) {
            case CLASS:
                final OWLClass owlClass = dataFactory().getOWLClass(IRI.create(axiom.getValue().stringValue()));
                owlAxiom = dataFactory().getOWLClassAssertionAxiom(owlClass, individual);
                break;
            case PROPERTY:
                // TODO
                break;
            case OBJECT_PROPERTY:
                final OWLObjectProperty objectProperty = dataFactory().getOWLObjectProperty(
                        IRI.create(axiom.getAssertion().getIdentifier()));
                final OWLNamedIndividual objectValue = dataFactory().getOWLNamedIndividual(
                        IRI.create(axiom.getValue().stringValue()));
                owlAxiom = dataFactory().getOWLObjectPropertyAssertionAxiom(objectProperty, individual, objectValue);
                break;
            case DATA_PROPERTY:
                final OWLDataProperty dataProperty = dataFactory().getOWLDataProperty(
                        IRI.create(axiom.getAssertion().getIdentifier()));
                final OWLLiteral dataValue = OwlapiUtils.createOWLLiteralFromValue(axiom.getValue().getValue(),
                        dataFactory(), language);
                owlAxiom = dataFactory().getOWLDataPropertyAssertionAxiom(dataProperty, individual, dataValue);
                break;
            case ANNOTATION_PROPERTY:
                final OWLAnnotationProperty annotationProperty = dataFactory().getOWLAnnotationProperty(IRI.create(
                        axiom.getAssertion().getIdentifier()));
                final OWLAnnotationValue annotationValue = OwlapiUtils.createOWLLiteralFromValue(
                        axiom.getValue().getValue(), dataFactory(), language);
                owlAxiom = dataFactory().getOWLAnnotationAssertionAxiom(annotationProperty, individual.getIRI(),
                        annotationValue);
                break;
        }
        return owlAxiom;
    }

    Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
        startTransactionIfNotActive();
        return new AxiomLoader(ontologySnapshot).findAxioms(descriptor);
    }
}
