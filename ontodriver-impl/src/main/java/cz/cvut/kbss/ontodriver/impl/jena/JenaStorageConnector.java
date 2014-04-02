package cz.cvut.kbss.ontodriver.impl.jena;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.coode.owlapi.turtle.TurtleOntologyFormat;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyFormat;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.util.OWLOntologyMerger;

import com.hp.hpl.jena.rdf.model.Model;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OntologyMutable;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;
import cz.cvut.kbss.ontodriver.impl.utils.OntoDriverConstants;

abstract class JenaStorageConnector implements OwlapiBasedJenaConnector {

	protected static final Logger LOG = Logger.getLogger(JenaStorageConnector.class.getName());

	protected final URI ontologyUri;
	protected final URI physicalUri;
	protected final String reasonerFactoryClass;
	private final String language;

	protected Model model;

	private boolean open;

	private OWLOntologyManager ontologyManager;
	private OWLReasonerFactory reasonerFactory;
	private OWLReasoner reasoner;
	private OWLOntology workingOntology;

	public JenaStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super();
		Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		Objects.requireNonNull(properties, ErrorUtils.constructNPXMessage("properties"));

		this.ontologyUri = storageProperties.getOntologyURI();
		this.physicalUri = storageProperties.getPhysicalURI();
		this.reasonerFactoryClass = properties
				.containsKey(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS) ? properties
				.get(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS)
				: OntoDriverConstants.REASONER_FACTORY_CLASS;
		this.language = properties.get(OntoDriverProperties.ONTOLOGY_LANGUAGE);
		this.initConnector();
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
		if (model != null) {
			model.close();
		}
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public OwlapiConnectorDataHolder getOntologyDataInOwlapi() throws OntoDriverException {
		final OwlapiConnectorDataHolder holder = transformToOwlapi();
		return holder;
	}

	@Override
	public int getClassAssertionAxiomsCount() {
		final int res = workingOntology == null ? 0 : workingOntology
				.getAxiomCount(AxiomType.CLASS_ASSERTION);
		return res;
	}

	@Override
	public OwlapiConnectorDataHolder cloneOntologyDataInOwlapi() {
		final OwlapiConnectorDataHolder holder = OwlapiConnectorDataHolder
				.ontologyManager(ontologyManager).dataFactory(ontologyManager.getOWLDataFactory())
				.reasoner(reasoner).workingOntology(workingOntology).build();
		return holder;
	}

	@Override
	public void applyOntologyChanges(List<OWLOntologyChange> changes) throws OntoDriverException {
		Objects.requireNonNull(changes, ErrorUtils.constructNPXMessage("changes"));
		if (changes.isEmpty()) {
			return;
		}
		assert ontologyManager != null;
		assert workingOntology != null;

		try {
			for (OWLOntologyChange change : changes) {
				if (change.getOntology().getOntologyID().equals(workingOntology.getOntologyID())) {
					assert (change instanceof OntologyMutable);
					((OntologyMutable) change).setOntology(workingOntology);
				}
			}
			ontologyManager.applyChanges(changes);

			final OWLOntologyFormat format = new TurtleOntologyFormat();
			ontologyManager.setOntologyFormat(workingOntology, format);
			final ByteArrayOutputStream bos = new ByteArrayOutputStream();
			ontologyManager.saveOntology(workingOntology, format, bos);

			final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
			model.removeAll();
			model.read(bis, null, OntoDriverConstants.TURTLE_FORMAT);
		} catch (OWLOntologyStorageException e) {
			LOG.log(Level.SEVERE, "Unable to transform OWL API ontology to Jena.", e);
			reload();
			throw new OntoDriverException(e);
		}
	}

	/**
	 * Transforms Jena ontology to OWL API ontology.
	 * 
	 * @return
	 * @throws OntoDriverException
	 */
	private OwlapiConnectorDataHolder transformToOwlapi() throws OntoDriverException {
		try {
			initOwlStructures();
			final ByteArrayOutputStream bos = new ByteArrayOutputStream();
			model.write(bos, OntoDriverConstants.TURTLE_FORMAT);

			final IRI ontoIri = IRI.create(ontologyUri);
			final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
			if (ontologyManager.contains(ontoIri)) {
				ontologyManager.removeOntology(workingOntology);
			}
			ontologyManager.loadOntologyFromOntologyDocument(bis);

			if (!ontologyManager.contains(ontoIri)) {
				OWLOntologyMerger merger = new OWLOntologyMerger(ontologyManager);
				this.workingOntology = merger.createMergedOntology(ontologyManager, ontoIri);
			} else {
				this.workingOntology = ontologyManager.getOntology(ontoIri);
			}

			this.reasoner = reasonerFactory.createReasoner(workingOntology);

			final OwlapiConnectorDataHolder data = OwlapiConnectorDataHolder
					.ontologyManager(ontologyManager)
					.dataFactory(ontologyManager.getOWLDataFactory())
					.workingOntology(workingOntology).reasoner(reasoner).language(language).build();
			return data;
		} catch (OWLOntologyCreationException e) {
			LOG.log(Level.SEVERE, "Unable to transform Jena ontology to OWL API.", e);
			throw new OntoDriverException(e);
		} catch (InstantiationException e) {
			LOG.log(Level.SEVERE, "Unable to instantiate reasoner factory class.", e);
			throw new OntoDriverException(e);
		} catch (IllegalAccessException e) {
			LOG.log(Level.SEVERE, "Unable to instantiate reasoner factory class.", e);
			throw new OntoDriverException(e);
		} catch (ClassNotFoundException e) {
			LOG.log(Level.SEVERE, "Unable to instantiate reasoner factory class.", e);
			throw new OntoDriverException(e);
		}
	}

	private void initOwlStructures() throws InstantiationException, IllegalAccessException,
			ClassNotFoundException {
		if (ontologyManager != null) {
			return;
		}
		this.ontologyManager = OWLManager.createOWLOntologyManager();
		this.reasonerFactory = (OWLReasonerFactory) Class.forName(reasonerFactoryClass)
				.newInstance();
	}

	/**
	 * Initializes this connector. </p>
	 * 
	 * This method is called by the constructor of {@code JenaStorageConnector}.
	 * 
	 * @throws OntoDriverException
	 *             If connector initialization fails
	 */
	protected abstract void initConnector() throws OntoDriverException;
}
