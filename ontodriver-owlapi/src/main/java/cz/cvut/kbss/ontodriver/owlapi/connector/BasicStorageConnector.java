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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.*;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAxiomChange;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.OntologyCopy;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;

import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Default file-based storage connector.
 * <p>
 * Each call to {@link #getOntologySnapshot()} returns a new snapshot of the current state of the ontology. The changes
 * are the applied to a shared ontology, which represents the current state of the underlying storage.
 * <p>
 * Note: This connector currently does not handle concurrent updates.
 */
public class BasicStorageConnector extends AbstractConnector {

    private static final Logger LOG = Logger.getLogger(BasicStorageConnector.class.getName());

    private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
    private static Lock READ = LOCK.readLock();
    private static Lock WRITE = LOCK.writeLock();

    private OWLOntologyManager ontologyManager;
    private OWLOntology ontology;
    private OWLReasoner reasoner;
    private OWLReasonerFactory reasonerFactory;

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
        initializeReasonerFactory();
        this.reasoner = getReasoner(ontology);
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

    private void initializeReasonerFactory() {
        final String reasonerFactoryClass = properties.get(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS);
        if (reasonerFactoryClass == null) {
            LOG.warning("Reasoner factory class not found. Reasoner won't be available.");
            return;
        }
        try {
            this.reasonerFactory = (OWLReasonerFactory) Class.forName(reasonerFactoryClass).newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            final String msg = "Unable to instantiate reasoner factory class " + reasonerFactoryClass;
            LOG.severe(msg);
            throw new ReasonerNotAvailableException(msg);
        } catch (ClassNotFoundException e) {
            final String msg = "Reasoner factory class " + reasonerFactoryClass + " not found!";
            LOG.severe(msg);
            throw new ReasonerNotAvailableException(msg, e);
        }
    }

    @Override
    public OntologySnapshot getOntologySnapshot() {
        ensureOpen();
        READ.lock();
        try {
            final OWLOntologyManager m = OWLManager.createOWLOntologyManager();
            final OWLOntology snapshot = m.copyOntology(ontology, OntologyCopy.DEEP);
            return new OntologySnapshot(snapshot, m, m.getOWLDataFactory(), getReasoner(snapshot));
        } catch (OWLOntologyCreationException e) {
            throw new OntologySnapshotException("Unable to create ontology snapshot.", e);
        } finally {
            READ.unlock();
        }
    }

    private OntologySnapshot getLiveOntology() {
        ensureOpen();
        return new OntologySnapshot(ontology, ontologyManager, ontologyManager.getOWLDataFactory(), reasoner);
    }

    @Override
    public <R> R executeRead(Function<OntologySnapshot, R> function) {
        ensureOpen();
        READ.lock();
        try {
            return function.apply(getLiveOntology());
        } finally {
            READ.unlock();
        }
    }

    @Override
    public void executeWrite(Consumer<OntologySnapshot> function) {
        ensureOpen();
        WRITE.lock();
        try {
            function.accept(getLiveOntology());
        } finally {
            WRITE.unlock();
        }
    }

    private OWLReasoner getReasoner(OWLOntology ontology) {
        if (reasonerFactory == null) {
            LOG.warning(
                    "Creating ontology snapshot without reasoner, because reasoner factory class was not specified.");
            return null;
        }
        return reasonerFactory.createReasoner(ontology);
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
