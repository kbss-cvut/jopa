package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.net.URI;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyID;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwldbOntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.OWLDBOntology;
import de.fraunhofer.iitb.owldb.OWLDBOntologyFormat;
import de.fraunhofer.iitb.owldb.OWLDBOntologyManager;
import de.fraunhofer.iitb.owldb.OWLDBOntologyOutputTarget;

/**
 * Implementation of OWLAPI storage connector for ontologies saved in RDBMS and
 * accessed through OWLDB.
 * 
 * @author kidney
 * 
 */
class OwlapiOwldbStorageConnector extends OwlapiStorageConnector {

	/** Hibernate database username property. */
	public static String HIBERNATE_USERNAME_PROPERTY = "hibernate.connection.username";
	/** Hibernate database password property. */
	public static String HIBERNATE_PASSWORD_PROPERTY = "hibernate.connection.password";
	/** Hibernate database URL property. */
	public static String HIBERNATE_JDBC_URL_PROPERTY = "hibernate.connection.url";
	/** Hibernate JDBC driver class property. */
	public static String HIBERNATE_JDBC_DRIVER_PROPERTY = "hibernate.connection.driver_class";

	private IRI databaseIri;

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
		this.databaseIri = IRI.create(physicalUri);

		final Properties props = new Properties();
		props.setProperty(HIBERNATE_JDBC_URL_PROPERTY, physicalUri.toString());
		if (storageProperties instanceof OwldbOntologyStorageProperties) {
			final OwldbOntologyStorageProperties owldbProps = (OwldbOntologyStorageProperties) storageProperties;
			initOwldbProperties(props, owldbProps);
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Using database backend: " + physicalUri);
		}
		this.ontologyManager = OWLDBManager.createOWLOntologyManager(OWLDataFactoryImpl
				.getInstance());

		setIriMapper(logicalUri, physicalUri);

		this.dataFactory = this.ontologyManager.getOWLDataFactory();

		try {
			this.workingOntology = ((OWLDBOntologyManager) ontologyManager).loadOntology(
					ontologyIri, props);
		} catch (OWLOntologyCreationException e) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Ontology " + logicalUri
						+ " does not exist in the database. Creating...");
			}
			createOntology(ontologyIri, props);
		}
		saveWorkingOntology();
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Number of individuals in signature: "
					+ workingOntology.getIndividualsInSignature().size());
		}
	}

	private void createOntology(IRI ontologyUri, Properties props) throws OntoDriverException {
		try {
			OWLOntologyID ontoId = new OWLOntologyID(ontologyUri);
			this.workingOntology = ((OWLDBOntologyManager) ontologyManager).createOntology(ontoId,
					props);
		} catch (OWLOntologyCreationException e) {
			throw new OntoDriverException(e);
		}
	}

	@Override
	public void saveWorkingOntology() throws OntoDriverException {
		// We must select the OWLDBOntologyFormat when storing in a
		// database
		final OWLDBOntologyFormat format = new OWLDBOntologyFormat();
		// Lets create a target with the provided target IRI
		final OWLDBOntologyOutputTarget target = new OWLDBOntologyOutputTarget(databaseIri);
		try {
			this.ontologyManager.saveOntology(workingOntology, format, target);
		} catch (OWLOntologyStorageException e) {
			throw new OntoDriverException(e);
		}
	}

	@Override
	public void close() throws OntoDriverException {
		super.close();
		((OWLDBOntology) workingOntology).destroyConnection();
	}

	/**
	 * Initializes properties for OWLDB's Hibernate backend based on the
	 * specified {@code OwldbOntologyStorageProperties} instance.
	 * 
	 * @param target
	 *            The target properties
	 * @param props
	 *            Properties data
	 */
	private static void initOwldbProperties(Properties target, OwldbOntologyStorageProperties props) {
		if (props.getUsername() != null) {
			target.setProperty(HIBERNATE_USERNAME_PROPERTY, props.getUsername());
		}
		if (props.getPassword() != null) {
			target.setProperty(HIBERNATE_PASSWORD_PROPERTY, props.getPassword());
		}
		if (props.getJdbcDriverClass() != null) {
			target.setProperty(HIBERNATE_JDBC_DRIVER_PROPERTY, props.getJdbcDriverClass());
			// Let's let Hibernate to decide the correct dialect from the JDBC
			// driver class
		}
	}
}
