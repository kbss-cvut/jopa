package cz.cvut.kbss.ontodriver;

import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;

/**
 * Holds properties of an ontology storage. </p>
 * 
 * These properties can be used to create a DataSource representing the storage.
 * 
 * @author kidney
 * 
 */
public class OntologyStorageProperties {

	/**
	 * URI of the ontology. </p>
	 * 
	 * Logical URI is not required, since for example for Sesame storages there
	 * is not logical URI, there is just the physical URI of the repository and
	 * multiple contexts in it. </p>
	 * 
	 * However, OWLAPI based storages require logical URI.
	 */
	private final URI ontologyUri;
	/** URI of the physical storage, e. g. OWLDB database, OWLIM storage, file */
	private final URI physicalUri;
	// TODO This will be removed, only driver class will be used
	/** Type of the storage connector. */
	private final OntologyConnectorType connectorType;
	/** Fully qualified OntoDriver data source class name */
	private final String driver;
	/** User name for the storage, if necessary */
	private final String username;
	/** Password for the storage, if neccessary */
	private final String password;

	OntologyStorageProperties(OntologyStoragePropertiesBuilder builder) {
		this.physicalUri = Objects.requireNonNull(builder.physicalUri,
				ErrorUtils.constructNPXMessage("physicalUri"));
		// TODO Will be removed
		this.connectorType = builder.connectorType;
		if (connectorType == null && builder.driverClass == null) {
			throw new IllegalStateException("Either data source or connector type has to be specified");
		}
		this.driver = builder.driverClass != null ? builder.driverClass : connectorType.getDataSource();
		this.ontologyUri = builder.ontologyUri;
		this.username = builder.username;
		this.password = builder.password;
	}

	public URI getOntologyURI() {
		return ontologyUri;
	}

	public URI getPhysicalURI() {
		return physicalUri;
	}

	public OntologyConnectorType getConnectorType() {
		return connectorType;
	}

	public String getDriver() {
		return driver;
	}

	public String getUsername() {
		return username;
	}

	public String getPassword() {
		return password;
	}

	@Override
	public String toString() {
		final StringBuilder b = new StringBuilder();
		b.append("StorageProperties: logical URI = ");
		b.append(ontologyUri);
		b.append(", physical URI = ");
		b.append(physicalUri);
		b.append(", data source class = ");
		b.append(driver);
		if (username != null) {
			b.append(", username = ");
			b.append(username);
			b.append(", password = ");
			b.append(password);
		}
		return b.toString();
	}

	public static OntologyStoragePropertiesBuilder ontologyUri(URI ontologyUri) {
		return new OntologyStoragePropertiesBuilder().ontologyUri(ontologyUri);
	}

	public static OntologyStoragePropertiesBuilder physicalUri(URI physicalUri) {
		return new OntologyStoragePropertiesBuilder().physicalUri(physicalUri);
	}

	public static OntologyStoragePropertiesBuilder connectorType(OntologyConnectorType connectorType) {
		return new OntologyStoragePropertiesBuilder().connectorType(connectorType);
	}

	public static OntologyStoragePropertiesBuilder username(String username) {
		return new OntologyStoragePropertiesBuilder().username(username);
	}

	public static OntologyStoragePropertiesBuilder password(String password) {
		return new OntologyStoragePropertiesBuilder().password(password);
	}

	public static OntologyStoragePropertiesBuilder driver(String driverClass) {
		return new OntologyStoragePropertiesBuilder().driver(driverClass);
	}

	/**
	 * Builder class for the {@code OntologyStorageProperties}.
	 * 
	 * @author kidney
	 * 
	 */
	public static class OntologyStoragePropertiesBuilder {

		protected URI ontologyUri;
		protected URI physicalUri;
		protected OntologyConnectorType connectorType;
		protected String username;
		protected String password;
		protected String driverClass;

		public OntologyStoragePropertiesBuilder ontologyUri(URI ontologyUri) {
			this.ontologyUri = ontologyUri;
			return this;
		}

		public OntologyStoragePropertiesBuilder physicalUri(URI physicalUri) {
			this.physicalUri = physicalUri;
			return this;
		}

		public OntologyStoragePropertiesBuilder connectorType(OntologyConnectorType connectorType) {
			this.connectorType = connectorType;
			return this;
		}

		public OntologyStoragePropertiesBuilder username(String username) {
			this.username = username;
			return this;
		}

		public OntologyStoragePropertiesBuilder password(String password) {
			this.password = password;
			return this;
		}

		public OntologyStoragePropertiesBuilder driver(String driverClass) {
			this.driverClass = driverClass;
			return this;
		}

		public OntologyStorageProperties build() {
			return new OntologyStorageProperties(this);
		}
	}
}
