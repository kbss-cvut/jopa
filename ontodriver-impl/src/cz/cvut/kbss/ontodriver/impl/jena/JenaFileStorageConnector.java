package cz.cvut.kbss.ontodriver.impl.jena;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat;
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

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.util.FileManager;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OntologyMutable;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;
import cz.cvut.kbss.ontodriver.impl.utils.OntoDriverConstants;

public class JenaFileStorageConnector implements OwlapiBasedJenaConnector {

	private static final Logger LOG = Logger.getLogger(JenaFileStorageConnector.class.getName());

	private final URI ontologyUri;
	private final URI physicalUri;
	private final String reasonerClassFactory;

	private OntModel model;

	private boolean open;

	private OWLOntologyManager ontologyManager;
	private OWLReasonerFactory reasonerFactory;
	private OWLReasoner reasoner;
	private OWLOntology workingOntology;

	public JenaFileStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super();
		if (storageProperties == null || properties == null) {
			throw new NullPointerException("Neither StorageProperties nor properties can be null.");
		}
		this.ontologyUri = storageProperties.getOntologyURI();
		this.physicalUri = storageProperties.getPhysicalURI();
		this.reasonerClassFactory = properties
				.containsKey(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS) ? properties
				.get(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS)
				: OntoDriverConstants.REASONER_FACTORY_CLASS;
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
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void reload() throws OntoDriverException {
		initConnector();
		// Force reloading of OWL API ontology
		getOntologyDataInOwlapi();
	}

	private void initConnector() throws OntoDriverException {
		this.model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		final File storageFile = new File(physicalUri);
		if (!storageFile.exists()) {
			try {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("Ontology file at location " + storageFile
							+ " does not exist. Creating empty file.");
				}
				storageFile.createNewFile();
				// No need to try loading model from input stream if the file
				// didn't exist
				return;
			} catch (IOException e) {
				LOG.log(Level.SEVERE, "Unable to create ontology file " + storageFile, e);
				throw new OntoDriverException(e);
			}
		}

		final InputStream in = FileManager.get().open(physicalUri.toString());
		if (in == null) {
			throw new OntoDriverException("Unable to load ontology in location " + physicalUri);
		}
		try {
			if (in.available() == 0) {
				// The file is empty
				return;
			}
		} catch (IOException e) {
			throw new OntoDriverException(e);
		}
		model.read(in, null);
	}

	public OntModel getModel() {
		return model;
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
		if (changes == null) {
			throw new NullPointerException();
		}
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

			final OWLOntologyFormat format = new RDFXMLOntologyFormat();
			ontologyManager.setOntologyFormat(workingOntology, format);
			final ByteArrayOutputStream bos = new ByteArrayOutputStream();
			ontologyManager.saveOntology(workingOntology, format, bos);

			final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
			model.close();
			this.model = ModelFactory.createOntologyModel();
			model.read(bis, null);
		} catch (OWLOntologyStorageException e) {
			LOG.log(Level.SEVERE, "Unable to transform OWL API ontology to Jena.", e);
			reload();
			throw new OntoDriverException(e);
		}
	}

	@Override
	public void saveOntology() throws OntoDriverException {
		assert model != null;
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving ontology " + ontologyUri + " to location " + physicalUri);
		}
		try {
			// We can assume that the file already exists, since it should have
			// been at least created by the initConnector method
			final OutputStream out = new FileOutputStream(new File(physicalUri));
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
			initOwlStructures();
			final ByteArrayOutputStream bos = new ByteArrayOutputStream();
			model.write(bos);

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
					.workingOntology(workingOntology).reasoner(reasoner).build();
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
		this.reasonerFactory = (OWLReasonerFactory) Class.forName(reasonerClassFactory)
				.newInstance();
	}
}
