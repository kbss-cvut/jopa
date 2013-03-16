package cz.cvut.kbss.ontodriver.impl.utils;

import java.net.URI;
import java.util.Properties;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwldbOntologyStorageProperties;

/**
 * Contains various utility methods for OWL API backed modules.
 * 
 * @author kidney
 * 
 */
public final class OwlapiUtils {

	/** Hibernate database username property. */
	private static String HIBERNATE_USERNAME_PROPERTY = "hibernate.connection.username";
	/** Hibernate database password property. */
	private static String HIBERNATE_PASSWORD_PROPERTY = "hibernate.connection.password";
	/** Hibernate database URL property. */
	private static String HIBERNATE_JDBC_URL_PROPERTY = "hibernate.connection.url";
	/** Hibernate JDBC driver class property. */
	private static String HIBERNATE_JDBC_DRIVER_PROPERTY = "hibernate.connection.driver_class";

	/**
	 * Private constructor.
	 */
	private OwlapiUtils() {
		throw new AssertionError();
	}

	/**
	 * Initializes database access properties for Hibernate backed OWLDB
	 * ontology connector. </p>
	 * 
	 * @param target
	 *            {@code Properties} instance in which the properties should be
	 *            written
	 * @param storageProperties
	 *            Information about the storage
	 * @throws NullPointerException
	 *             If {@code target} or {@code storageProperties} is
	 *             {@code null}
	 */
	public static void initHibernateProperties(Properties target,
			OntologyStorageProperties storageProperties) {
		if (target == null || storageProperties == null) {
			throw new NullPointerException();
		}
		target.setProperty(HIBERNATE_JDBC_URL_PROPERTY, storageProperties.getPhysicalURI()
				.toString());
		if (storageProperties.getUsername() != null) {
			target.setProperty(HIBERNATE_USERNAME_PROPERTY, storageProperties.getUsername());
		}
		if (storageProperties.getPassword() != null) {
			target.setProperty(HIBERNATE_PASSWORD_PROPERTY, storageProperties.getPassword());
		}
		if (storageProperties instanceof OwldbOntologyStorageProperties) {
			final OwldbOntologyStorageProperties osp = (OwldbOntologyStorageProperties) storageProperties;
			if (osp.getJdbcDriverClass() != null) {
				target.setProperty(HIBERNATE_JDBC_DRIVER_PROPERTY, osp.getJdbcDriverClass());
				// Let's let Hibernate decide the correct dialect from the JDBC
				// driver class
			}
		}
	}

	/**
	 * Sets onotlogy IRI mapper for the specified ontology manager. </p>
	 * 
	 * The IRI mapper maps logical URI of the ontology to the physical URI of
	 * the storage.
	 * 
	 * @param manager
	 *            The ontology manager
	 * @param logicalUri
	 *            Logical URI of the ontology
	 * @param physicalUri
	 *            Physical URI of the storage
	 */
	public static void setOntologyManagerIriMapper(OWLOntologyManager manager,
			final URI logicalUri, final URI physicalUri) {
		manager.addIRIMapper(new OWLOntologyIRIMapper() {
			public IRI getDocumentIRI(IRI arg0) {
				if (!logicalUri.equals(arg0.toURI())) {
					return arg0;
				}
				return IRI.create(physicalUri);
			}
		});
	}
}
