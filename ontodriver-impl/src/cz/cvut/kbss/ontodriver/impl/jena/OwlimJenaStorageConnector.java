package cz.cvut.kbss.ontodriver.impl.jena;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.http.HTTPRepository;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.OWLOntologyMerger;

import com.hp.hpl.jena.rdf.model.Model;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;

public class OwlimJenaStorageConnector implements OwlapiBasedJenaConnector {

	private static final Logger LOG = Logger.getLogger(OwlimJenaStorageConnector.class.getName());

	private final URI ontologyUri;
	private Repository repository;
	private RepositoryConnection connection;

	private Model model;
	private OWLOntologyManager ontologyManager;
	private OWLReasoner reasoner;
	private OWLOntology workingOntology;

	private boolean open;

	public OwlimJenaStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) {
		if (storageProperties == null || properties == null) {
			throw new NullPointerException();
		}
		this.open = true;
		this.ontologyUri = storageProperties.getOntologyURI();
		try {
			initialize(storageProperties, properties);
		} catch (RepositoryException e) {
			throw new OntoDriverInitializationException(e);
		}
	}

	private void initialize(OntologyStorageProperties storageProps, Map<String, String> props)
			throws RepositoryException {
		assert props != null;
		final URI physicalUri = storageProps.getPhysicalURI();
		this.repository = new HTTPRepository(physicalUri.toString());
		this.connection = repository.getConnection();
		// Create the DatasetGraph instance
		// SesameDataset dataset = new SesameDataset(connection);
		// Model model =
		// ModelFactory.createModelForGraph(dataset.getDefaultGraph());

		// TODO How to implement this?
	}

	@Override
	public void reload() throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void close() throws OntoDriverException {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing OWLIM storage connector.");
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
	public OwlapiConnectorDataHolder cloneOntologyDataInOwlapi() throws OntoDriverException {
		final OwlapiConnectorDataHolder holder = OwlapiConnectorDataHolder
				.ontologyManager(ontologyManager).dataFactory(ontologyManager.getOWLDataFactory())
				.reasoner(reasoner).workingOntology(workingOntology).build();
		return holder;
	}

	@Override
	public void applyOntologyChanges(List<OWLOntologyChange> changes) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void saveOntology() throws OntoDriverException {
		// TODO Auto-generated method stub

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

			// this.reasoner = reasonerFactory.createReasoner(workingOntology);

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
	}
}
