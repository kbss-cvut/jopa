package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.net.URI;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

import org.hibernate.HibernateException;
import org.hibernate.cfg.Configuration;
import org.hibernate.tool.hbm2ddl.SchemaExport;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyID;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.utils.OwlapiUtils;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.OWLDBOntology;
import de.fraunhofer.iitb.owldb.OWLDBOntologyManager;

/**
 * Implementation of OWLAPI storage connector for ontologies saved in RDBMS and
 * accessed through OWLDB.
 * 
 * @author kidney
 * 
 */
class OwlapiOwldbStorageConnector extends OwlapiStorageConnector {

	private static final String HIBERNATE_DIALECT = "hibernate.dialect";

	// private IRI databaseIri;
	private Properties hibernateProperties;

	public OwlapiOwldbStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(storageProperties, properties);
	}

	@Override
	protected void initConnection(OntologyStorageProperties storageProperties)
			throws OntoDriverException {
		final URI logicalUri = storageProperties.getOntologyURI();
		final URI physicalUri = storageProperties.getPhysicalURI();
		final IRI ontologyIri = IRI.create(logicalUri);
		// this.databaseIri = IRI.create(physicalUri);

		this.hibernateProperties = new Properties();
		OwlapiUtils.initHibernateProperties(hibernateProperties, storageProperties);

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Using database backend: " + physicalUri);
		}
		this.ontologyManager = OWLDBManager.createOWLOntologyManager(OWLDataFactoryImpl
				.getInstance());

		OwlapiUtils.setOntologyManagerIriMapper(ontologyManager, logicalUri, physicalUri);

		this.dataFactory = this.ontologyManager.getOWLDataFactory();

		try {
			this.workingOntology = ((OWLDBOntologyManager) ontologyManager).loadOntology(
					ontologyIri, hibernateProperties);
		} catch (OWLOntologyCreationException e) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Ontology " + logicalUri
						+ " does not exist in the database. Creating...");
			}
			createOntology(ontologyIri);
		} catch (HibernateException e) {
			if (LOG.isLoggable(Level.FINER)) {
				LOG.finer("OWLDB database structure not present. Generating...");
			}
			createOntology(ontologyIri);
		}
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Number of individuals in signature: "
					+ workingOntology.getIndividualsInSignature().size());
		}
	}

	@Override
	protected void reloadOntology() throws OntoDriverException {
		try {
			this.workingOntology = ((OWLDBOntologyManager) ontologyManager).loadOntology(
					IRI.create(ontologyUri), hibernateProperties);
		} catch (OWLOntologyCreationException e) {
			throw new OntoDriverException(e);
		}
	}

	private void createOntology(IRI ontologyUri) throws OntoDriverException {
		try {
			generateOwldbSchema();
			OWLOntologyID ontoId = new OWLOntologyID(ontologyUri);
			this.workingOntology = ((OWLDBOntologyManager) ontologyManager).createOntology(ontoId,
					hibernateProperties);
		} catch (OWLOntologyCreationException e) {
			throw new OntoDriverException(e);
		}
	}

	@Override
	public void saveWorkingOntology() throws OntoDriverException {
		// // We must select the OWLDBOntologyFormat when storing in a
		// // database
		// final OWLDBOntologyFormat format = new OWLDBOntologyFormat();
		// // Lets create a target with the provided target IRI
		// // final OWLDBOntologyOutputTarget target = new
		// // OWLDBOntologyOutputTarget(databaseIri);
		// try {
		// ((OWLDBOntologyManager) ontologyManager).saveOntology(
		// workingOntology, format, databaseIri, hibernateProperties);
		// // this.ontologyManager.saveOntology(workingOntology, format,
		// // target);
		// } catch (OWLOntologyStorageException e) {
		// throw new OntoDriverException(e);
		// } catch (OWLOntologyCreationException e) {
		// throw new OntoDriverException(e);
		// }
	}

	@Override
	public void close() throws OntoDriverException {
		super.close();
		((OWLDBOntology) workingOntology).destroyConnection();
	}

	/**
	 * Exports the OWLDB database schema into the database. </p>
	 */
	private void generateOwldbSchema() {
		LOG.config("Generating OWLDB database schema.");
		final Configuration cfg = new Configuration();
		cfg.configure();
		final String dialect = OwlapiUtils.resolveHibernateDialect(hibernateProperties);
		hibernateProperties.setProperty(HIBERNATE_DIALECT, dialect);
		cfg.addProperties(hibernateProperties);
		final SchemaExport ex = new SchemaExport(cfg);
		ex.create(false, true);
	}
}
