/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiConfigParam;
import cz.cvut.kbss.ontodriver.owlapi.exception.*;
import cz.cvut.kbss.ontodriver.owlapi.util.DefaultOntologyIriMapper;
import cz.cvut.kbss.ontodriver.owlapi.util.MappingFileParser;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAxiomChange;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileNotFoundException;
import java.net.URI;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Default file-based storage connector.
 * <p>
 * Each call to {@link #getOntologySnapshot()} returns a new snapshot of the current state of the ontology. The changes
 * are the applied to a shared ontology, which represents the current state of the underlying storage.
 * <p>
 * Note: This connector currently does not handle concurrent updates.
 */
public class BasicStorageConnector extends AbstractConnector {

    private static final Logger LOG = LoggerFactory.getLogger(BasicStorageConnector.class);

    private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
    private static final Lock READ = LOCK.readLock();
    private static final Lock WRITE = LOCK.writeLock();

    private OWLOntologyManager ontologyManager;
    private OWLOntology ontology;
    private OWLReasoner reasoner;
    private OWLReasonerFactory reasonerFactory;

    private OWLOntologyIRIMapper iriMapper;

    BasicStorageConnector(DriverConfiguration configuration) throws OwlapiDriverException {
        super(configuration);
    }

    @Override
    protected void initializeConnector() throws OwlapiDriverException {
        if (isOpen()) {
            return;
        }
        final OntologyStorageProperties storageProperties = configuration.getStorageProperties();
        LOG.debug("Loading ontology {} from {}.", storageProperties.getOntologyURI(),
                  storageProperties.getPhysicalURI());
        resolveIriMapper();
        this.ontologyManager = OWLManager.createOWLOntologyManager();
        setIriMapper(ontologyManager);
        loadOntology(storageProperties);
        initializeReasonerFactory();
        this.reasoner = getReasoner(ontology);
    }

    private void resolveIriMapper() {
        if (configuration.isSet(OwlapiConfigParam.MAPPING_FILE_LOCATION)) {
            this.iriMapper = new DefaultOntologyIriMapper(new MappingFileParser(configuration));
        }
    }

    private void setIriMapper(OWLOntologyManager manager) {
        if (iriMapper != null) {
            manager.getIRIMappers().add(new DefaultOntologyIriMapper(new MappingFileParser(configuration)));
        }
    }

    private void loadOntology(OntologyStorageProperties storageProperties) throws OwlapiDriverException {
        final Optional<IRI> ontologyIri = storageProperties.getOntologyURI().map(IRI::create);
        try {
            this.ontology =
                    ontologyManager.loadOntologyFromOntologyDocument(IRI.create(storageProperties.getPhysicalURI()));
            if (!ontology.getOntologyID().getOntologyIRI().equals(ontologyIri)) {
                throw new InvalidOntologyIriException(
                        "Expected ontology with IRI " + storageProperties.getOntologyURI() +
                                " but the loaded ontology has IRI " + ontology.getOntologyID().getOntologyIRI());
            }
        } catch (OWLOntologyCreationException e) {
            if (e.getCause() instanceof FileNotFoundException) {
                LOG.trace("Ontology file {} does not exist.", storageProperties.getPhysicalURI());
            } else {
                LOG.trace("Unable to load ontology from document.", e);
            }
            tryCreatingOntology(ontologyIri.orElse(null));
        }
    }

    private void tryCreatingOntology(IRI ontologyIri) throws OwlapiDriverException {
        LOG.trace("Creating new ontology in {}.", configuration.getStorageProperties().getPhysicalURI());
        try {
            this.ontology = ontologyManager.createOntology(ontologyIri);
            ontology.saveOntology(IRI.create(configuration.getStorageProperties().getPhysicalURI()));
        } catch (OWLOntologyCreationException | OWLOntologyStorageException e) {
            throw new OwlapiDriverException(
                    "Unable to create ontology in " + configuration.getStorageProperties().getPhysicalURI(), e);
        }
    }

    private void initializeReasonerFactory() {
        final String reasonerFactoryClass = configuration.getProperty(DriverConfigParam.REASONER_FACTORY_CLASS);
        if (reasonerFactoryClass == null) {
            LOG.warn("Reasoner factory class not found. Reasoner won't be available.");
            return;
        }
        try {
            this.reasonerFactory = (OWLReasonerFactory) Class.forName(reasonerFactoryClass).newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            final String msg = "Unable to instantiate reasoner factory class " + reasonerFactoryClass;
            LOG.error(msg);
            throw new ReasonerNotAvailableException(msg, e);
        } catch (ClassNotFoundException e) {
            final String msg = "Reasoner factory class " + reasonerFactoryClass + " not found!";
            LOG.error(msg);
            throw new ReasonerNotAvailableException(msg, e);
        }
    }

    @Override
    public URI getOntologyUri() {
        assert ontology.getOntologyID().getOntologyIRI().isPresent();

        return ontology.getOntologyID().getOntologyIRI().get().toURI();
    }

    @Override
    public OntologySnapshot getOntologySnapshot() {
        ensureOpen();
        READ.lock();
        try {
            final OWLOntology snapshot = ontologyManager.createOntology();
            cloneOntologyContent(snapshot);
            return new OntologySnapshot(snapshot, ontologyManager, ontologyManager.getOWLDataFactory(),
                                        getReasoner(snapshot));
        } catch (OWLOntologyCreationException e) {
            throw new OntologySnapshotException("Unable to create ontology snapshot.", e);
        } finally {
            READ.unlock();
        }
    }

    private void cloneOntologyContent(OWLOntology target) {
        ontologyManager.addAxioms(target, ontology.axioms());
        ontologyManager.applyChanges(
                ontology.importsDeclarations().map(i -> new AddImport(target, i)).collect(Collectors.toList()));
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
            LOG.warn("Creating ontology snapshot without reasoner, because reasoner factory class was not specified.");
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
            try {
                writeToFile();
            } catch (OntologyStorageException e) {
                LOG.error("Unable to write out ontology." + e);
            }
        } finally {
            WRITE.unlock();
        }
    }

    @Override
    public void closeSnapshot(OntologySnapshot snapshot) {
        ensureOpen();
        assert snapshot != null;
        ontologyManager.removeOntology(snapshot.getOntology());
    }

    @Override
    void reloadData() throws OwlapiDriverException {
        WRITE.lock();
        try {
            ontologyManager.clearOntologies();
            loadOntology(configuration.getStorageProperties());
            this.reasoner = getReasoner(ontology);
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
            writeToFile();
            super.close();
        } finally {
            WRITE.unlock();
        }
    }

    private void writeToFile() throws OntologyStorageException {
        try {
            ontologyManager.saveOntology(ontology, IRI.create(configuration.getStorageProperties().getPhysicalURI()));
        } catch (OWLOntologyStorageException e) {
            throw new OntologyStorageException(
                    "Error when saving ontology to " + configuration.getStorageProperties().getPhysicalURI(), e);
        }
    }
}
