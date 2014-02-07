package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import org.hibernate.dialect.HSQLDialect;
import org.hibernate.dialect.MySQLDialect;
import org.hibernate.dialect.Oracle10gDialect;
import org.hibernate.dialect.PostgreSQLDialect;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwldbOntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.JDBCDriverNotSpecifiedException;
import cz.cvut.kbss.ontodriver.exceptions.UnsupportedOwldbDriverException;

/**
 * Contains various utility methods for OWL API backed modules.
 * 
 * @author kidney
 * 
 */
public final class OwlapiUtils {

	/** Hibernate database username property. */
	private static final String HIBERNATE_USERNAME_PROPERTY = "hibernate.connection.username";
	/** Hibernate database password property. */
	private static final String HIBERNATE_PASSWORD_PROPERTY = "hibernate.connection.password";
	/** Hibernate database URL property. */
	private static final String HIBERNATE_JDBC_URL_PROPERTY = "hibernate.connection.url";
	/** Hibernate JDBC driver class property. */
	private static final String HIBERNATE_JDBC_DRIVER_PROPERTY = "hibernate.connection.driver_class";

	private static final String POSTGRES_JDBC = "org.postgresql.Driver";
	private static final String MYSQL_JDBC = "com.mysql.jdbc.Driver";
	private static final String ORACLE_JDBC = "oracle.jdbc.OracleDriver";
	private static final String HSQL_JDBC = "org.hsqldb.jdbc.JDBCDriver";

	/**
	 * Private constructor.
	 */
	private OwlapiUtils() {
		throw new AssertionError();
	}

	/**
	 * Creates OWLLiteral from the specified Java instance.
	 * 
	 * @param value
	 *            The value to transform
	 * @param dataFactory
	 *            Data factory
	 * @param lang
	 *            Ontology language
	 * @return OWLLiteral representing the value
	 * @throws IllegalArgumentException
	 *             If {@code value} is of unsupported type
	 */
	public static OWLLiteral createOWLLiteralFromValue(Object value, OWLDataFactory dataFactory,
			String lang) {
		// No need to check for null since instanceof returns always false for
		// null
		if (value instanceof Integer) {
			return dataFactory.getOWLLiteral((Integer) value);
		} else if (value instanceof Boolean) {
			return dataFactory.getOWLLiteral((Boolean) value);
		} else if (value instanceof Double) {
			return dataFactory.getOWLLiteral((Double) value);
		} else if (value instanceof String) {
			return dataFactory.getOWLLiteral((String) value, lang);
		} else if (value instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");
			return dataFactory.getOWLLiteral(sdf.format(((Date) value)),
					dataFactory.getOWLDatatype(OWL2Datatype.XSD_DATE_TIME.getIRI()));
		} else {
			throw new IllegalArgumentException();
		}
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
	 * Returns Hibernate dialect class name for the specified JDBC driver class.
	 * 
	 * @param properties
	 *            Properties containing JDBC driver class name
	 * @return Hibernate dialect name
	 * @throws JDBCDriverNotSpecifiedException
	 *             If the properties contain no JDBC driver specification
	 * @throws UnsupportedOwldbDriverException
	 *             Unsupported JDBC driver passed in properties
	 */
	public static String resolveHibernateDialect(Properties properties) {
		final String jdbcDriver = properties.getProperty(HIBERNATE_JDBC_DRIVER_PROPERTY);
		if (jdbcDriver == null) {
			throw new JDBCDriverNotSpecifiedException(
					"Missing JDBC driver class in OWLDB connector.");
		}
		if (jdbcDriver.equals(POSTGRES_JDBC)) {
			return PostgreSQLDialect.class.getCanonicalName();
		} else if (jdbcDriver.equals(MYSQL_JDBC)) {
			return MySQLDialect.class.getCanonicalName();
		} else if (jdbcDriver.equals(ORACLE_JDBC)) {
			return Oracle10gDialect.class.getCanonicalName();
		} else if (jdbcDriver.equals(HSQL_JDBC)) {
			return HSQLDialect.class.getCanonicalName();
		} else {
			throw new UnsupportedOwldbDriverException(
					"Unable to resolve hibernate dialect for driver " + jdbcDriver);
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
