package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.InvalidOntologyIriException;
import cz.cvut.kbss.ontodriver.owlapi.util.IdentifierGenerator;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.OntoDriverProperties;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.exception.OWLIndividualExistsException;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
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

    private enum TransactionState {
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
        final Collection<OWLAxiom> owlAxiom = asOwlAxioms(axiom);
        boolean contains;
        for (OWLAxiom ax : owlAxiom) {
            if (axiom.getAssertion().isInferred()) {
                contains = reasoner().isEntailed(ax);
            } else {
                contains = ontology().containsAxiom(ax);
            }
            if (contains) {
                return true;
            }
        }
        return false;
    }

    private Collection<OWLAxiom> asOwlAxioms(Axiom<?> axiom) {
        final Collection<OWLAxiom> owlAxioms = new ArrayList<>(3);
        switch (axiom.getAssertion().getType()) {
            case CLASS:
                owlAxioms.add(asClassAssertionAxiom(axiom));
                break;
            case PROPERTY:
                owlAxioms.add(asClassAssertionAxiom(axiom));
                owlAxioms.add(asObjectAxiom(axiom));
                owlAxioms.add(asDataAxiom(axiom));
                owlAxioms.add(asAnnotationAxiom(axiom));
                break;
            case OBJECT_PROPERTY:
                owlAxioms.add(asObjectAxiom(axiom));
                break;
            case DATA_PROPERTY:
                owlAxioms.add(asDataAxiom(axiom));
                break;
            case ANNOTATION_PROPERTY:
                owlAxioms.add(asAnnotationAxiom(axiom));
                break;
        }
        return owlAxioms;
    }

    private OWLAxiom asClassAssertionAxiom(Axiom<?> axiom) {
        final OWLClass owlClass = dataFactory().getOWLClass(IRI.create(axiom.getValue().stringValue()));
        return dataFactory().getOWLClassAssertionAxiom(owlClass, individual(axiom.getSubject()));
    }

    private OWLAxiom asObjectAxiom(Axiom<?> axiom) {
        final OWLObjectProperty objectProperty = dataFactory().getOWLObjectProperty(
                IRI.create(axiom.getAssertion().getIdentifier()));
        final OWLNamedIndividual objectValue = dataFactory().getOWLNamedIndividual(
                IRI.create(axiom.getValue().stringValue()));
        return dataFactory().getOWLObjectPropertyAssertionAxiom(objectProperty, individual(axiom.getSubject()),
                objectValue);
    }

    private OWLAxiom asDataAxiom(Axiom<?> axiom) {
        final OWLDataProperty dataProperty = dataFactory().getOWLDataProperty(
                IRI.create(axiom.getAssertion().getIdentifier()));
        final OWLLiteral dataValue = OwlapiUtils.createOWLLiteralFromValue(axiom.getValue().getValue(),
                dataFactory(), language);
        return dataFactory().getOWLDataPropertyAssertionAxiom(dataProperty, individual(axiom.getSubject()), dataValue);
    }

    private OWLAxiom asAnnotationAxiom(Axiom<?> axiom) {
        final OWLAnnotationProperty annotationProperty = dataFactory().getOWLAnnotationProperty(IRI.create(
                axiom.getAssertion().getIdentifier()));
        final OWLAnnotationValue annotationValue = OwlapiUtils.createOWLLiteralFromValue(
                axiom.getValue().getValue(), dataFactory(), language);
        return dataFactory().getOWLAnnotationAssertionAxiom(annotationProperty, individual(axiom.getSubject()).getIRI(),
                annotationValue);
    }

    private OWLNamedIndividual individual(NamedResource subject) {
        return dataFactory().getOWLNamedIndividual(IRI.create(subject.getIdentifier()));
    }

    Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
        startTransactionIfNotActive();
        return new AxiomLoader(this, ontologySnapshot).findAxioms(descriptor);
    }

    void persist(AxiomValueDescriptor descriptor) {
        startTransactionIfNotActive();
        if (individualExists(descriptor.getSubject())) {
            throw new OWLIndividualExistsException(
                    "Individual " + descriptor.getSubject() + " already exists in the ontology.");
        }
        new AxiomSaver(this, ontologySnapshot, language).persist(descriptor);
    }

    private boolean individualExists(NamedResource subject) {
        return ontology().containsIndividualInSignature(IRI.create(subject.getIdentifier()));
    }

    URI generateIdentifier(URI classUri) {
        startTransactionIfNotActive();
        return new IdentifierGenerator(ontology()).generateIdentifier(classUri);
    }

    TypesHandler getTypesHandler() {
        startTransactionIfNotActive();
        return new TypesHandler(this, ontologySnapshot);
    }

    void addTransactionalChanges(List<OWLOntologyChange> changes) {
        pendingChanges.addAll(changes);
    }
}
