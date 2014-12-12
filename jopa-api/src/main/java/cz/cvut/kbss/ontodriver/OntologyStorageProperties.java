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
	// TODO This will be removed, only dataSource class will be used
	/** Type of the storage connector. */
	private final OntologyConnectorType connectorType;
	/** Fully qualified OntoDriver data source class name */
	private final String dataSource;
	/** User name for the storage, if necessary */
	private final String username;
	/** Password for the storage, if neccessary */
	private final String password;

	/**
	 * Constructor for the OntologyStorageProperties.
	 * 
	 * @param ontologyUri
	 *            URI of the ontology
	 * @param physicalUri
	 *            URI of the storage where the ontology is stored
	 * @param connectorType
	 *            Type of the connector
	 * @throws NullPointerException
	 *             If {@code physicalUri} or {@code connectorType} is null
	 */
	public OntologyStorageProperties(URI ontologyUri, URI physicalUri,
			OntologyConnectorType connectorType) {
		super();
		this.physicalUri = Objects.requireNonNull(physicalUri,
				ErrorUtils.constructNPXMessage("physicalUri"));
		this.connectorType = Objects.requireNonNull(connectorType,
				ErrorUtils.constructNPXMessage("connectorType"));
		this.dataSource = connectorType.getDataSource();
		this.ontologyUri = ontologyUri;
		this.username = null;
		this.password = null;
	}

	/**
	 * Constructor for the OntologyStorageProperties.
	 * 
	 * @param ontologyUri
	 *            URI of the ontology
	 * @param physicalUri
	 *            URI of the storage where the ontology is stored
	 * @param connectorType
	 *            Type of the ontology connector
	 * @param username
	 *            Username for the storage. Optional
	 * @param password
	 *            Password for the storage. Optional
	 * @throws NullPointerException
	 *             If {@code physicalUri} or {@code connectorType} is
	 *             {@code null}
	 * @see #OntologyStorageProperties(URI, URI)
	 */
	public OntologyStorageProperties(URI ontologyUri, URI physicalUri,
			OntologyConnectorType connectorType, String username, String password) {
		super();
		this.physicalUri = Objects.requireNonNull(physicalUri,
				ErrorUtils.constructNPXMessage("physicalUri"));
		this.connectorType = Objects.requireNonNull(connectorType,
				ErrorUtils.constructNPXMessage("connectorType"));
		this.dataSource = connectorType.getDataSource();
		this.ontologyUri = ontologyUri;
		this.username = username;
		this.password = password;
	}

	protected OntologyStorageProperties(OntologyStoragePropertiesBuilder builder) {
		this(builder.ontologyUri, builder.physicalUri, builder.connectorType, builder.username,
				builder.password);
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

	public String getDataSource() {
		return dataSource;
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
		b.append(", connector type = ");
		b.append(connectorType);
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

		public OntologyStorageProperties build() {
			return new OntologyStorageProperties(this);
		}
	}
}
