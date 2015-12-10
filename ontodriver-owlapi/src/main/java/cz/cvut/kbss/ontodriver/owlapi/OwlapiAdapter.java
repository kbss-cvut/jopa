package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.InvalidOntologyIriException;
import cz.cvut.kbss.ontodriver.owlapi.list.ListHandler;
import cz.cvut.kbss.ontodriver.owlapi.query.OwlapiPreparedStatement;
import cz.cvut.kbss.ontodriver.owlapi.query.OwlapiStatement;
import cz.cvut.kbss.ontodriver.owlapi.query.StatementExecutorFactory;
import cz.cvut.kbss.ontodriver.owlapi.util.IdentifierGenerator;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.exception.OWLIndividualExistsException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
import java.util.*;

/**
 * Adapter between OntoDriver API and OWLAPI.
 *
 * @author ledvima1
 */
public class OwlapiAdapter {

    private final Connector connector;
    private OntologySnapshot ontologySnapshot;
    private final String language;

    private StatementExecutorFactory statementExecutorFactory;

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
            this.statementExecutorFactory = new StatementExecutorFactory(ontologySnapshot, connector);
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
        final AxiomAdapter axiomAdapter = new AxiomAdapter(dataFactory(), language);
        switch (axiom.getAssertion().getType()) {
            case CLASS:
                owlAxioms.add(axiomAdapter.toOwlClassAssertionAxiom(axiom));
                break;
            case PROPERTY:
                owlAxioms.add(axiomAdapter.toOwlClassAssertionAxiom(axiom));
                owlAxioms.add(axiomAdapter.toOwlObjectPropertyAssertionAxiom(axiom));
                owlAxioms.add(axiomAdapter.toOwlDataPropertyAssertionAxiom(axiom));
                owlAxioms.add(axiomAdapter.toOwlAnnotationPropertyAssertionAxiom(axiom));
                break;
            case OBJECT_PROPERTY:
                owlAxioms.add(axiomAdapter.toOwlObjectPropertyAssertionAxiom(axiom));
                break;
            case DATA_PROPERTY:
                owlAxioms.add(axiomAdapter.toOwlDataPropertyAssertionAxiom(axiom));
                break;
            case ANNOTATION_PROPERTY:
                owlAxioms.add(axiomAdapter.toOwlAnnotationPropertyAssertionAxiom(axiom));
                break;
        }
        return owlAxioms;
    }

    Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
        startTransactionIfNotActive();
        return new MainAxiomLoader(this, ontologySnapshot).findAxioms(descriptor);
    }

    void persist(AxiomValueDescriptor descriptor) {
        startTransactionIfNotActive();
        if (instanceExists(descriptor.getSubject())) {
            throw new OWLIndividualExistsException(
                    "Individual " + descriptor.getSubject() + " already exists in the ontology.");
        }
        new AxiomSaver(this, ontologySnapshot).persist(descriptor);
    }

    /**
     * An individual has to have explicit type(s) to exist in this sense.
     */
    private boolean instanceExists(NamedResource subject) {
        final IRI iri = IRI.create(subject.getIdentifier());
        if (!ontology().containsIndividualInSignature(iri)) {
            return false;
        }
        final OWLNamedIndividual ind = dataFactory().getOWLNamedIndividual(iri);
        return (!EntitySearcher.getTypes(ind, ontology()).isEmpty());
    }

    URI generateIdentifier(URI classUri) {
        startTransactionIfNotActive();
        return new IdentifierGenerator(ontology()).generateIdentifier(classUri);
    }

    void update(AxiomValueDescriptor descriptor) {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(this, ontologySnapshot).remove(descriptor);
        new AxiomSaver(this, ontologySnapshot).persist(descriptor);
    }

    void remove(AxiomDescriptor descriptor) {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(this, ontologySnapshot).remove(descriptor);
    }

    TypesHandler getTypesHandler() {
        startTransactionIfNotActive();
        return new TypesHandler(this, ontologySnapshot);
    }

    PropertiesHandler getPropertiesHandler() {
        startTransactionIfNotActive();
        return new PropertiesHandler(this, ontologySnapshot);
    }

    public void addTransactionalChanges(Collection<OWLOntologyChange> changes) {
        pendingChanges.addAll(changes);
    }

    public String getLanguage() {
        return language;
    }

    public ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> getSimpleListHandler() {
        startTransactionIfNotActive();
        return ListHandler.getSimpleListHandler(this, ontologySnapshot);
    }

    public ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> getReferencedListHandler() {
        startTransactionIfNotActive();
        return ListHandler.getReferencedListHandler(this, ontologySnapshot);
    }

    public OwlapiStatement createStatement(OwlapiConnection connection) {
        startTransactionIfNotActive();
        return new OwlapiStatement(statementExecutorFactory, connection);
    }

    public OwlapiPreparedStatement prepareStatement(String statement, OwlapiConnection connection) {
        startTransactionIfNotActive();
        return new OwlapiPreparedStatement(statementExecutorFactory, connection, statement);
    }
}
