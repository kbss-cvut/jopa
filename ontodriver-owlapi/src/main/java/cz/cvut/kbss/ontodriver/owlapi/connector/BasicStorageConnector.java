package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.InvalidOntologyIriException;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.OntologySnapshotException;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.OntologyStorageException;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAxiomChange;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.OntologyCopy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * Default file-based storage connector.
 * <p/>
 * Each call to {@link #getOntologySnapshot()} returns a new snapshot of the current state of the ontology. The changes
 * are the applied to a shared ontology, which represents the current state of the underlying storage.
 */
public class BasicStorageConnector extends Connector {

    private static final Logger LOG = LoggerFactory.getLogger(BasicStorageConnector.class);

    private OWLOntologyManager ontologyManager;
    private OWLOntology ontology;

    public BasicStorageConnector(OntologyStorageProperties storageProperties, Map<String, String> properties) throws
            OwlapiDriverException {
        super(storageProperties, properties);
    }

    @Override
    protected void initializeConnector() throws OwlapiDriverException {
        if (LOG.isTraceEnabled()) {
            LOG.trace(
                    "Loading ontology " + storageProperties.getOntologyURI() + " from " + storageProperties.getPhysicalURI());
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
        if (LOG.isDebugEnabled()) {
            LOG.debug("Creating new ontology in " + storageProperties.getPhysicalURI());
        }
        try {
            this.ontology = ontologyManager.createOntology(IRI.create(storageProperties.getOntologyURI()));
            ontology.saveOntology(IRI.create(storageProperties.getPhysicalURI()));
        } catch (OWLOntologyCreationException | OWLOntologyStorageException e) {
            throw new OwlapiDriverException("Unable to create ontology in " + storageProperties.getPhysicalURI(), e);
        }
    }

    @Override
    public synchronized OntologyStructures getOntologySnapshot() {
        ensureOpen();
        try {
            final OWLOntologyManager m = OWLManager.createOWLOntologyManager();
            final OWLOntology snapshot = m.copyOntology(ontology, OntologyCopy.DEEP);
            return new OntologyStructures(snapshot, m, m.getOWLDataFactory());
        } catch (OWLOntologyCreationException e) {
            throw new OntologySnapshotException("Unable to create ontology snapshot.", e);
        }
    }

    @Override
    public synchronized void applyChanges(List<OWLOntologyChange> changes) {
        ensureOpen();
        assert changes != null;
        for (OWLOntologyChange ch : changes) {
            if (ch instanceof MutableAxiomChange) {
                ((MutableAxiomChange) ch).setOntology(ontology);
            }
        }
        ontologyManager.applyChanges(changes);
    }

    @Override
    public void close() throws OntoDriverException {
        if (!isOpen()) {
            return;
        }
        try {
            ontologyManager.saveOntology(ontology, IRI.create(storageProperties.getPhysicalURI()));
        } catch (OWLOntologyStorageException e) {
            throw new OntologyStorageException("Error when saving ontology to " + storageProperties.getPhysicalURI(),
                    e);
        }
        super.close();
    }
}
