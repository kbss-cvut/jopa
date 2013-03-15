package cz.cvut.kbss.ontodriver.impl.jena;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyFormat;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.util.OWLOntologyMerger;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.util.FileManager;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;
import cz.cvut.kbss.ontodriver.impl.utils.OntoDriverConstants;

public class JenaStorageConnector implements StorageConnector {

	private static final Logger LOG = Logger.getLogger(JenaStorageConnector.class.getName());

	private final URI ontologyUri;
	private final URI physicalUri;
	private final String reasonerClassFactory;

	private OntModel model;

	private boolean open;

	public JenaStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super();
		if (storageProperties == null) {
			throw new NullPointerException("StorageProperties cannot be null.");
		}
		this.ontologyUri = storageProperties.getOntologyURI();
		this.physicalUri = storageProperties.getPhysicalURI();
		this.reasonerClassFactory = properties
				.containsKey(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS) ? properties
				.get(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS)
				: OntoDriverConstants.REASONER_FACTORY_CLASS;
		initConnector(storageProperties, properties);
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

	private void initConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		assert storageProperties != null;
		assert properties != null;
		this.model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		final InputStream in = FileManager.get()
				.open(storageProperties.getPhysicalURI().toString());
		if (in == null) {
			throw new OntoDriverException("Unable to load ontology in location "
					+ storageProperties.getPhysicalURI());
		}
		model.read(in, null);
	}

	/**
	 * Retrieves the Jena ontology model.
	 * 
	 * @return Jena model
	 */
	public OntModel getModel() {
		return model;
	}

	/**
	 * Retrieves the ontology in OWL API structures. </p>
	 * 
	 * This method internally performs a transformation from Jena model to OWL
	 * API data structures so that OWL API internals can be used to work with
	 * the data.
	 * 
	 * @return {@link OwlapiConnectorDataHolder}
	 * @throws OntoDriverException
	 *             If an error during transformation occurs
	 */
	public OwlapiConnectorDataHolder getOntologyDataInOwlapi() throws OntoDriverException {
		final OwlapiConnectorDataHolder holder = transformToOwlapi();
		return holder;
	}

	/**
	 * Applies changes made to the {@code ontology} to the Jena ontology model
	 * held by this connector. </p>
	 * 
	 * Note that by calling this method the original model held by this
	 * connector is closed and recreated from the specified {@code ontology}.
	 * </p>
	 * 
	 * Also note that calling this method does not save the changes into the
	 * underlying storage, this has to be done by explicitly calling
	 * {@link #saveOntology()}.
	 * 
	 * @param manager
	 *            {@code OWLOntologyManager}
	 * @param ontology
	 *            {@code OWLOntology} containing changes
	 * @throws OntoDriverException
	 *             IF an error during OWL API -> Jena transformation occurs
	 */
	public void applyOntologyChanges(OWLOntologyManager manager, OWLOntology ontology)
			throws OntoDriverException {
		if (manager == null || ontology == null) {
			throw new NullPointerException();
		}
		try {
			final OWLOntologyFormat format = new RDFXMLOntologyFormat();
			manager.setOntologyFormat(ontology, format);
			final ByteArrayOutputStream bos = new ByteArrayOutputStream();
			manager.saveOntology(ontology, format, bos);

			final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
			model.close();
			this.model = ModelFactory.createOntologyModel();
			model.read(bis, null);
		} catch (OWLOntologyStorageException e) {
			LOG.log(Level.SEVERE, "Unable to transform OWL API ontology to Jena.", e);
			throw new OntoDriverException(e);
		}
	}

	/**
	 * Outputs the ontology into its physical location.
	 * 
	 * @throws OntoDriverException
	 */
	public void saveOntology() throws OntoDriverException {
		assert model != null;
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving ontology " + ontologyUri + " to location " + physicalUri);
		}
		try {
			// We can assume that the file already exists, since it should have
			// been at least created by the initConnector method
			final OutputStream out = new FileOutputStream(physicalUri.toString());
			model.write(out);
		} catch (FileNotFoundException e) {
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
			final ByteArrayOutputStream bos = new ByteArrayOutputStream();
			model.write(bos);

			final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
			final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
			manager.loadOntologyFromOntologyDocument(bis);

			OWLOntologyMerger merger = new OWLOntologyMerger(manager);
			final IRI ontoIri = IRI.create(ontologyUri);
			final OWLOntology ontology = merger.createMergedOntology(manager, ontoIri);

			final OWLReasonerFactory reasonerFactory = (OWLReasonerFactory) Class.forName(
					reasonerClassFactory).newInstance();
			final OWLReasoner reasoner = reasonerFactory.createReasoner(ontology);

			final OwlapiConnectorDataHolder data = OwlapiConnectorDataHolder
					.ontologyManager(manager).dataFactory(manager.getOWLDataFactory())
					.workingOntology(ontology).reasoner(reasoner).build();
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
}
