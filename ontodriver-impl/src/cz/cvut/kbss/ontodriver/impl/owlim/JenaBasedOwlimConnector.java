package cz.cvut.kbss.ontodriver.impl.owlim;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.OpenRDFException;
import org.openrdf.model.Graph;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.impl.GraphImpl;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.config.RepositoryConfig;
import org.openrdf.repository.manager.LocalRepositoryManager;
import org.openrdf.repository.manager.RepositoryManager;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;
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

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.ontotext.jena.SesameDataset;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OwlimOntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.jena.OwlapiBasedJenaConnector;
import cz.cvut.kbss.ontodriver.impl.owlapi.OntologyMutable;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;
import cz.cvut.kbss.ontodriver.impl.utils.OntoDriverConstants;

/**
 * This OWLIM connector will work only for locally running OWLIM repositories.
 * 
 * @author kidney
 * 
 */
public class JenaBasedOwlimConnector implements OwlapiBasedJenaConnector {

	private static final Logger LOG = Logger.getLogger(JenaBasedOwlimConnector.class.getName());

	private static final String REPOSITORY_URI = "http://www.openrdf.org/config/repository#Repository";
	private static final String REPOSITORY_ID_URI = "http://www.openrdf.org/config/repository#repositoryID";

	private boolean open;

	// Sesame API
	private RepositoryManager repositoryManager;
	private Repository repository;
	private RepositoryConnection connection;

	// Jena API
	private Model jenaModel;

	// OWL API
	private OWLOntologyManager ontologyManager;
	private OWLReasonerFactory reasonerFactory;
	private OWLReasoner reasoner;
	private OWLOntology workingOntology;

	private final OwlimOntologyStorageProperties props;
	private final String reasonerFactoryClass;

	public JenaBasedOwlimConnector(OwlimOntologyStorageProperties storageProps,
			Map<String, String> properties) throws OntoDriverException {
		if (storageProps == null) {
			throw new NullPointerException();
		}
		this.props = storageProps;
		if (properties == null) {
			throw new NullPointerException();
		}
		this.reasonerFactoryClass = properties
				.containsKey(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS) ? properties
				.get(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS)
				: OntoDriverConstants.REASONER_FACTORY_CLASS;
		initializeConnector();
	}

	private void initializeConnector() throws OntoDriverException {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Initializing OWLIM repository connector.");
		}
		final File owlimConfig = new File(props.getOwlimConfigFile());
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Using OWLIM configuration file " + owlimConfig.getAbsolutePath());
		}
		Graph repositoryDescription = null;
		try {
			repositoryDescription = parseConfigFile(owlimConfig, RDFFormat.TURTLE,
					"http://www.example.org");
		} catch (OpenRDFException e) {
			LOG.severe("Unable to parse the configuration file. Make sure that it is in Turtle format. Error: "
					+ e.getMessage());
			throw new OntoDriverException(e);
		} catch (IOException e) {
			LOG.severe("An I/O exception occured when reading the configuration file "
					+ props.getOwlimConfigFile() + ": " + e.getMessage());
			throw new OntoDriverException(e);
		}

		final Resource repositoryNode = getRepositoryNode(repositoryDescription);
		final String repositoryId = getRepositoryId(repositoryDescription, repositoryNode);
		try {
			// Create a manager for local repositories and initialize it
			this.repositoryManager = new LocalRepositoryManager(new File(props.getPhysicalURI()));
			repositoryManager.initialize();
			// Create a configuration object from the configuration file and add
			// it to the repositoryManager
			final RepositoryConfig repositoryConfig = RepositoryConfig.create(
					repositoryDescription, repositoryNode);
			repositoryManager.addRepositoryConfig(repositoryConfig);
		} catch (OpenRDFException e) {
			LOG.severe("Unable to process the repository configuration: " + e.getMessage());
			throw new OntoDriverException(e);
		}
		// Get the repository to use
		try {
			this.repository = repositoryManager.getRepository(repositoryId);
			if (repository == null) {
				throw new OntoDriverException("Repository with id " + repositoryId
						+ " is not reachable in the current repository manager.");
			}
			// Open a connection to this repository
			this.connection = repository.getConnection();
			connection.setAutoCommit(false);
			// Transfer the Sesame repository to a Jena model
			SesameDataset dataset = new SesameDataset(connection);
			this.jenaModel = ModelFactory.createModelForGraph(dataset.getDefaultGraph());
		} catch (OpenRDFException e) {
			LOG.severe("Unable to establish a connection to the repository '" + repositoryId
					+ "': " + e.getMessage());
			throw new OntoDriverException(e);
		}
	}

	@Override
	public void reload() throws OntoDriverException {
		// Do nothing
	}

	@Override
	public void close() throws OntoDriverException {
		LOG.config("Closing OWLIM storage connector.");
		try {
			jenaModel.close();
			connection.close();
			repository.shutDown();
			repositoryManager.shutDown();
		} catch (RepositoryException e) {
			throw new OntoDriverException(e);
		}

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
		if (changes == null) {
			throw new NullPointerException();
		}
		if (changes.isEmpty()) {
			return;
		}
		assert ontologyManager != null;
		assert workingOntology != null;
		try {
			try {
				for (OWLOntologyChange change : changes) {
					if (change.getOntology().getOntologyID()
							.equals(workingOntology.getOntologyID())) {
						assert (change instanceof OntologyMutable);
						((OntologyMutable) change).setOntology(workingOntology);
					}
				}
				ontologyManager.applyChanges(changes);

				final OWLOntologyFormat format = new RDFXMLOntologyFormat();
				ontologyManager.setOntologyFormat(workingOntology, format);
				final ByteArrayOutputStream bos = new ByteArrayOutputStream();
				ontologyManager.saveOntology(workingOntology, format, bos);

				// We'll see how this works out.
				final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
				jenaModel.removeAll();
				jenaModel.read(bis, null);
				connection.commit();
			} catch (OWLOntologyStorageException e) {
				LOG.log(Level.SEVERE, "Unable to transform OWL API ontology to Jena.", e);
				connection.rollback();
				throw new OntoDriverException(e);
			} catch (RepositoryException e) {
				LOG.log(Level.SEVERE, "Unable to apply changes to OWLIM repository.", e);
				connection.rollback();
				throw new OntoDriverException(e);
			}
		} catch (RepositoryException e) {
			throw new OntoDriverException(e);
		}
	}

	@Override
	public void saveOntology() throws OntoDriverException {
		// Do nothing, the changes are automatically propagated to OWLIM
	}

	private static Graph parseConfigFile(File configFile, RDFFormat format, String defaultNamespace)
			throws RDFParseException, RDFHandlerException, IOException {
		Reader reader = new FileReader(configFile);

		final Graph graph = new GraphImpl();
		RDFParser parser = Rio.createParser(format);
		RDFHandler handler = new RDFHandler() {
			@Override
			public void endRDF() throws RDFHandlerException {
			}

			@Override
			public void handleComment(String arg0) throws RDFHandlerException {
			}

			@Override
			public void handleNamespace(String arg0, String arg1) throws RDFHandlerException {
			}

			@Override
			public void handleStatement(Statement statement) throws RDFHandlerException {
				graph.add(statement);
			}

			@Override
			public void startRDF() throws RDFHandlerException {
			}
		};
		parser.setRDFHandler(handler);
		parser.parse(reader, defaultNamespace);
		return graph;
	}

	private Resource getRepositoryNode(Graph repositoryDescription) {
		Iterator<Statement> iter = repositoryDescription.match(null, RDF.TYPE, new URIImpl(
				REPOSITORY_URI));
		Resource repositoryNode = null;
		if (iter.hasNext()) {
			Statement st = iter.next();
			repositoryNode = st.getSubject();
		} else {
			LOG.severe("The configuration file " + props.getOwlimConfigFile()
					+ " does not contain valid repository description.");
		}
		return repositoryNode;
	}

	private String getRepositoryId(Graph repositoryDescription, Resource repositoryNode) {
		// Get the repository ID (and ignore the one passed with the
		// 'repository' parameter
		String repositoryId = null;
		final Iterator<Statement> iter = repositoryDescription.match(repositoryNode, new URIImpl(
				REPOSITORY_ID_URI), null);
		if (iter.hasNext()) {
			Statement st = iter.next();
			repositoryId = st.getObject().stringValue();
		} else {
			LOG.severe("The configuration file " + props.getOwlimConfigFile()
					+ " does not contain valid repository description containing repository id.");
		}
		return repositoryId;
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
			jenaModel.write(bos);

			final IRI ontoIri = IRI.create(props.getOntologyURI());
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
		this.reasonerFactory = (OWLReasonerFactory) Class.forName(reasonerFactoryClass)
				.newInstance();
	}
}
