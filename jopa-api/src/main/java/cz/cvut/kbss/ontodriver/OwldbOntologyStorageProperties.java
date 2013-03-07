package cz.cvut.kbss.ontodriver;

import java.net.URI;

/**
 * Adds possibility to specify JDBC driver class for the underlying OWLDB
 * storage connector. </p>
 * 
 * Based on the {@code physicalUri} and the driver class the target database is
 * determined and connection to it is established. Note that corresponding JDBC
 * driver has to be present on classpath before initializing the storage
 * connection.
 * 
 * @author kidney
 * 
 */
public final class OwldbOntologyStorageProperties extends OntologyStorageProperties {

	private final String jdbcDriverClass;

	public OwldbOntologyStorageProperties(URI ontologyUri, URI physicalUri,
			OntologyConnectorType connectorType, String jdbcDriverClass) {
		super(ontologyUri, physicalUri, connectorType);
		if (jdbcDriverClass == null || jdbcDriverClass.isEmpty()) {
			throw new IllegalArgumentException(
					"The jdbc driver class cannot be neither null nor empty.");
		}
		this.jdbcDriverClass = jdbcDriverClass;
	}

	public OwldbOntologyStorageProperties(OwldbStoragePropertiesBuilder builder) {
		super(builder.ontologyUri, builder.physicalUri, builder.connectorType, builder.username,
				builder.password);
		if (builder.jdbcDriverClass == null || builder.jdbcDriverClass.isEmpty()) {
			throw new IllegalArgumentException(
					"The jdbc driver class cannot be neither null nor empty.");
		}
		this.jdbcDriverClass = builder.jdbcDriverClass;
	}

	/**
	 * Retrieves name of the JDBC driver class. </p>
	 * 
	 * The driver class should correspond to the {@code physicalUri} and the
	 * JDBC driver itself should be on classpath.
	 * 
	 * @return Name of the JDBC driver class
	 */
	public String getJdbcDriverClass() {
		return jdbcDriverClass;
	}

	public static OwldbStoragePropertiesBuilder ontologyUri(URI ontologyUri) {
		return new OwldbStoragePropertiesBuilder().ontologyUri(ontologyUri);
	}

	public static OwldbStoragePropertiesBuilder physicalUri(URI physicalUri) {
		return new OwldbStoragePropertiesBuilder().physicalUri(physicalUri);
	}

	public static OwldbStoragePropertiesBuilder connectorType(OntologyConnectorType connectorType) {
		return new OwldbStoragePropertiesBuilder().connectorType(connectorType);
	}

	public static OwldbStoragePropertiesBuilder jdbcDriverClass(String jdbcDriverClass) {
		return new OwldbStoragePropertiesBuilder().jdbcDriverClass(jdbcDriverClass);
	}

	public static OwldbStoragePropertiesBuilder username(String username) {
		return new OwldbStoragePropertiesBuilder().username(username);
	}

	public static OwldbStoragePropertiesBuilder password(String password) {
		return new OwldbStoragePropertiesBuilder().password(password);
	}

	/**
	 * Builder class for the {@code OwldbOntologyStorageProperties}.
	 * 
	 * @author kidney
	 * 
	 */
	public static class OwldbStoragePropertiesBuilder {

		private URI ontologyUri;
		private URI physicalUri;
		private OntologyConnectorType connectorType;
		private String jdbcDriverClass;
		private String username;
		private String password;

		public OwldbStoragePropertiesBuilder ontologyUri(URI ontologyUri) {
			this.ontologyUri = ontologyUri;
			return this;
		}

		public OwldbStoragePropertiesBuilder physicalUri(URI physicalUri) {
			this.physicalUri = physicalUri;
			return this;
		}

		public OwldbStoragePropertiesBuilder connectorType(OntologyConnectorType connectorType) {
			this.connectorType = connectorType;
			return this;
		}

		public OwldbStoragePropertiesBuilder jdbcDriverClass(String jdbcDriverClass) {
			this.jdbcDriverClass = jdbcDriverClass;
			return this;
		}

		public OwldbStoragePropertiesBuilder username(String username) {
			this.username = username;
			return this;
		}

		public OwldbStoragePropertiesBuilder password(String password) {
			this.password = password;
			return this;
		}

		public OwldbOntologyStorageProperties build() {
			return new OwldbOntologyStorageProperties(this);
		}
	}
}
