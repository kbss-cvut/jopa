package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyRenameException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public abstract class OwlapiStorageConnector implements StorageConnector, OwlapiConnector {

	protected static final Logger LOG = Logger.getLogger(OwlapiStorageConnector.class.getName());

	protected final URI ontologyUri;
	protected URI physicalUri;

	protected OWLReasoner reasoner;
	protected OWLOntology workingOntology;
	protected OWLOntologyManager ontologyManager;
	protected OWLReasonerFactory reasonerFactory;
	protected OWLDataFactory dataFactory;
	protected String language;

	private boolean open;

	/**
	 * Constructor for inheritance. </p>
	 * 
	 * Sets only ontology URI.
	 */
	protected OwlapiStorageConnector(URI ontologyUri, URI physicalUri) {
		if (ontologyUri == null || physicalUri == null) {
			throw new NullPointerException();
		}
		this.ontologyUri = ontologyUri;
		this.physicalUri = physicalUri;
	}

	public OwlapiStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		this.ontologyUri = storageProperties.getOntologyURI();
		this.physicalUri = storageProperties.getPhysicalURI();
		this.language = properties.get(OntoDriverProperties.ONTOLOGY_LANGUAGE);
		final String reasonerFactoryClass = properties
				.get(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS);
		try {
			this.reasonerFactory = (OWLReasonerFactory) Class.forName(reasonerFactoryClass)
					.newInstance();
		} catch (Exception e) {
			throw new OntoDriverException("Error instantiating factory " + reasonerFactoryClass
					+ ".", e);
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.info("Loading model ontologyURI=" + storageProperties.getOntologyURI()
					+ ", physicalURI=" + physicalUri + ".");
		}
		try {
			initConnection(storageProperties);

			LOG.info("Ontology " + ontologyUri + " succesfully loaded.");
		} catch (Exception e) {
			LOG.log(Level.SEVERE, null, e);
			throw new OntoDriverException(e);
		}
		try {
			this.reasoner = this.reasonerFactory.createReasoner(workingOntology);
			this.reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing storage connector.");
		}
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void reload() throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Reloading ontology from the storage.");
		}
		reloadInternal();
	}

	public OWLReasoner getReasoner() {
		ensureOpen();
		return reasoner;
	}

	public OWLOntology getWorkingOntology() {
		ensureOpen();
		return workingOntology;
	}

	public OWLOntologyManager getOntologyManager() {
		ensureOpen();
		return ontologyManager;
	}

	public OWLReasonerFactory getReasonerFactory() {
		ensureOpen();
		return reasonerFactory;
	}

	public OWLDataFactory getDataFactory() {
		ensureOpen();
		return dataFactory;
	}

	/**
	 * Gets data holder containing original connector. </p>
	 * 
	 * In contrast to {@link #cloneOntologyData()} this method returns the
	 * original ontology structures.
	 * 
	 * @return OwlapiConnectorDataHolder
	 * @see #cloneOntologyData()
	 */
	OwlapiConnectorDataHolder getOntologyData() {
		ensureOpen();
		final OwlapiConnectorDataHolder holder = OwlapiConnectorDataHolder
				.workingOntology(workingOntology).ontologyManager(ontologyManager)
				.dataFactory(dataFactory).language(language).reasoner(reasoner).build();
		return holder;
	}

	/**
	 * Clones the working ontology into a new in-memory ontology. </p>
	 * 
	 * Besides the working ontology a new OWLOntologyManager with OWLDataFactory
	 * is created. Reasoner and reasoning ontology are reused from this
	 * connector.
	 * 
	 * @return OwlapiConnectorDataHolder
	 * @throws OntoDriverException
	 *             If an error during data cloning occurs
	 */
	OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException {
		ensureOpen();
		final Set<OWLAxiom> axioms = getWorkingOntology().getAxioms();
		final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		final OWLDataFactory factory = manager.getOWLDataFactory();
		OWLOntology ontology = null;
		try {
			ontology = manager.createOntology(axioms, IRI.create(ontologyUri));
		} catch (OWLOntologyCreationException e) {
			throw new OntoDriverException("Unable to clone working ontology.", e);
		}
		final OWLReasoner reasoner = reasonerFactory.createReasoner(ontology);
		final OwlapiConnectorDataHolder holder = OwlapiConnectorDataHolder
				.workingOntology(ontology).ontologyManager(manager).dataFactory(factory)
				.reasoner(reasoner).language(language).build();
		return holder;
	}

	/**
	 * Applies the specified changes to the working ontology. </p>
	 * 
	 * The changes are applied but the ontology has to be explicitly saved
	 * before the changes become persistent.
	 * 
	 * @param changes
	 *            The changes to apply
	 * @throws OntoDriverException
	 *             If an error during application of changes occurs
	 */
	@Override
	public void applyChanges(List<OWLOntologyChange> changes) throws OntoDriverException {
		ensureOpen();
		final List<OWLOntologyChange> applyFirst = new ArrayList<OWLOntologyChange>();
		for (OWLOntologyChange change : changes) {
			if (change.getOntology().getOntologyID().equals(workingOntology.getOntologyID())) {
				assert (change instanceof OntologyMutable);
				((OntologyMutable) change).setOntology(workingOntology);
			}
			// TODO Fix transaction isolation, when one transaction modifies an
			// attribute and the other
			// tries to modify it afterwards, there should be policy to handle
			// it: e. g. for Serializable
			// level the second transaction should be rolled back with an
			// exception
		}
		try {
			ontologyManager.applyChanges(applyFirst);
			ontologyManager.applyChanges(changes);
		} catch (OWLOntologyRenameException e) {
			reload();
			throw new OntoDriverException(e);
		}
	}

	/**
	 * Returns the number of class assertions in the working ontology.
	 * 
	 * @return Number of class assertions
	 */
	@Override
	public int getClassAssertionsCount() {
		return workingOntology.getAxiomCount(AxiomType.CLASS_ASSERTION);
	}

	private void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("The connector is closed.");
		}
	}

	private void reloadInternal() throws OntoDriverException {
		ontologyManager.removeOntology(workingOntology);
		reloadOntology();
		try {
			this.reasoner = reasonerFactory.createReasoner(workingOntology);
			this.reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
		} catch (Exception e) {
			throw new OntoDriverException("Unable to reload the ontology " + ontologyUri, e);
		}
	}

	/**
	 * Saves the working ontology. </p>
	 * 
	 * Saving the working ontology makes changes in it persistent in the
	 * underlying storage.
	 * 
	 * @throws OntoDriverException
	 *             If an error during the save operation occurs
	 */
	@Override
	public void saveWorkingOntology() throws OntoDriverException {
		try {
			ontologyManager.saveOntology(workingOntology);
		} catch (OWLOntologyStorageException e) {
			throw new OntoDriverException(e);
		}
	}

	/**
	 * Reloads the working ontology from the storage. </p>
	 * 
	 * @throws OntoDriverException
	 *             If an error during reload occurs
	 */
	protected abstract void reloadOntology() throws OntoDriverException;

	/**
	 * Initializes connection to the storage. </p>
	 * 
	 * This procedure may differ according to the storage type.
	 * 
	 * @param storageProperties
	 *            Storage properties
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	protected abstract void initConnection(OntologyStorageProperties storageProperties)
			throws OntoDriverException;
}
