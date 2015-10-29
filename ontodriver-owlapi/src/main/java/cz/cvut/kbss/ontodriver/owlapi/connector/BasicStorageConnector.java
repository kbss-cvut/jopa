package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.InvalidOntologyIriException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OntologySnapshotException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OntologyStorageException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAxiomChange;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.OntologyCopy;

import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Default file-based storage connector.
 * <p>
 * Each call to {@link #getOntologySnapshot()} returns a new snapshot of the current state of the ontology. The changes
 * are the applied to a shared ontology, which represents the current state of the underlying storage.
 *
 * Note: This connector currently does not handle concurrent updates.
 */
public class BasicStorageConnector extends AbstractConnector {

    private static final Logger LOG = Logger.getLogger(BasicStorageConnector.class.getName());

    private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
    private static Lock READ = LOCK.readLock();
    private static Lock WRITE = LOCK.writeLock();

    private OWLOntologyManager ontologyManager;
    private OWLOntology ontology;

    public BasicStorageConnector(OntologyStorageProperties storageProperties, Map<String, String> properties) throws
            OwlapiDriverException {
        super(storageProperties, properties);
    }

    @Override
    protected void initializeConnector() throws OwlapiDriverException {
        if (isOpen()) {
            return;
        }
        if (LOG.isLoggable(Level.CONFIG)) {
            LOG.config("Loading ontology " + storageProperties.getOntologyURI() + " from " +
                    storageProperties.getPhysicalURI());
        }
        this.ontologyManager = OWLManager.createOWLOntologyManager();
        try {
            this.ontology = ontologyManager.loadOntologyFromOntologyDocument(
                    IRI.create(storageProperties.getPhysicalURI()));
            if (!ontology.getOntologyID().getOntologyIRI().isPresent() ||
                    !ontology.getOntologyID().getOntologyIRI().get().equals(
                            IRI.create(storageProperties.getOntologyURI()))) {
                throw new InvalidOntologyIriException(
                        "Expected ontology with IRI " + storageProperties.getOntologyURI() +
                                " but the loaded ontology has IRI " + ontology.getOntologyID().getOntologyIRI());
            }
        } catch (OWLOntologyCreationException e) {
            tryCreatingOntology();
        }
    }

    private void tryCreatingOntology() throws OwlapiDriverException {
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("Creating new ontology in " + storageProperties.getPhysicalURI());
        }
        try {
            this.ontology = ontologyManager.createOntology(IRI.create(storageProperties.getOntologyURI()));
            ontology.saveOntology(IRI.create(storageProperties.getPhysicalURI()));
        } catch (OWLOntologyCreationException | OWLOntologyStorageException e) {
            throw new OwlapiDriverException("Unable to create ontology in " + storageProperties.getPhysicalURI(), e);
        }
    }

    @Override
    public OntologySnapshot getOntologySnapshot() {
        ensureOpen();
        READ.lock();
        try {
            // TODO init reasoner
            final OWLOntologyManager m = OWLManager.createOWLOntologyManager();
            final OWLOntology snapshot = m.copyOntology(ontology, OntologyCopy.DEEP);
            return new OntologySnapshot(snapshot, m, m.getOWLDataFactory(), null);
        } catch (OWLOntologyCreationException e) {
            throw new OntologySnapshotException("Unable to create ontology snapshot.", e);
        } finally {
            READ.unlock();
        }
    }

    @Override
    public void applyChanges(List<OWLOntologyChange> changes) {
        ensureOpen();
        assert changes != null;
        WRITE.lock();
        try {
            changes.stream().filter(ch -> ch instanceof MutableAxiomChange)
                   .forEach(ch -> ((MutableAxiomChange) ch).setOntology(ontology));
            ontologyManager.applyChanges(changes);
        } finally {
            WRITE.unlock();
        }
    }

    @Override
    public void close() throws OntoDriverException {
        if (!isOpen()) {
            return;
        }
        WRITE.lock();
        try {
            try {
                ontologyManager.saveOntology(ontology, IRI.create(storageProperties.getPhysicalURI()));
            } catch (OWLOntologyStorageException e) {
                throw new OntologyStorageException(
                        "Error when saving ontology to " + storageProperties.getPhysicalURI(), e);
            }
            super.close();
        } finally {
            WRITE.unlock();
        }
    }
}
