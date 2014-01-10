package cz.cvut.kbss.ontodriver;

import java.net.URI;

/**
 * Storage properties for OWLIM repositories. </p>
 * 
 * It enables providing path to a OWLIM configuration file.
 * 
 * @author kidney
 * 
 */
public final class OwlimOntologyStorageProperties extends OntologyStorageProperties {

	/** OWLIM configuration file path */
	private final String owlimConfigFile;

	public OwlimOntologyStorageProperties(URI ontologyUri, URI physicalUri,
			OntologyConnectorType connectorType) {
		super(ontologyUri, physicalUri, connectorType);
		this.owlimConfigFile = null;
	}

	public OwlimOntologyStorageProperties(URI ontologyUri, URI physicalUri,
			OntologyConnectorType connectorType, String username, String password) {
		super(ontologyUri, physicalUri, connectorType, username, password);
		this.owlimConfigFile = null;
	}

	public OwlimOntologyStorageProperties(URI ontologyUri, URI physicalUri,
			OntologyConnectorType connectorType, String username, String password,
			String owlimConfigFile) {
		super(ontologyUri, physicalUri, connectorType, username, password);
		if (owlimConfigFile == null) {
			throw new NullPointerException();
		}
		this.owlimConfigFile = owlimConfigFile;
	}

	private OwlimOntologyStorageProperties(OwlimStoragePropertiesBuilder builder) {
		super(builder);
		this.owlimConfigFile = builder.owlimConfigFile;
	}

	/**
	 * Gets path to the OWLIM repository configuration file.
	 * 
	 * @return File path as string
	 */
	public String getOwlimConfigFile() {
		return owlimConfigFile;
	}

	@Override
	public String toString() {
		final StringBuilder b = new StringBuilder(super.toString());
		b.append(", OWLIM Config file = ").append(owlimConfigFile);
		return b.toString();
	}

	public static OwlimStoragePropertiesBuilder ontologyUri(URI ontologyUri) {
		return new OwlimStoragePropertiesBuilder().ontologyUri(ontologyUri);
	}

	public static OwlimStoragePropertiesBuilder physicalUri(URI physicalUri) {
		return new OwlimStoragePropertiesBuilder().physicalUri(physicalUri);
	}

	public static OwlimStoragePropertiesBuilder connectorType(OntologyConnectorType connectorType) {
		return new OwlimStoragePropertiesBuilder().connectorType(connectorType);
	}

	public static OwlimStoragePropertiesBuilder username(String username) {
		return new OwlimStoragePropertiesBuilder().username(username);
	}

	public static OwlimStoragePropertiesBuilder password(String password) {
		return new OwlimStoragePropertiesBuilder().password(password);
	}

	public static OwlimStoragePropertiesBuilder owlimConfigFile(String owlimConfigFile) {
		return new OwlimStoragePropertiesBuilder().owlimConfigFile(owlimConfigFile);
	}

	/**
	 * Builder class for the {@code OwlimOntologyStorageProperties} class.
	 * 
	 * @author kidney
	 * 
	 */
	public static final class OwlimStoragePropertiesBuilder extends
			OntologyStoragePropertiesBuilder {

		private String owlimConfigFile;

		@Override
		public OwlimStoragePropertiesBuilder ontologyUri(URI ontologyUri) {
			super.ontologyUri(ontologyUri);
			return this;
		}

		@Override
		public OwlimStoragePropertiesBuilder physicalUri(URI physicalUri) {
			super.physicalUri(physicalUri);
			return this;
		}

		@Override
		public OwlimStoragePropertiesBuilder connectorType(OntologyConnectorType connectorType) {
			super.connectorType(connectorType);
			return this;
		}

		@Override
		public OwlimStoragePropertiesBuilder username(String username) {
			super.username(username);
			return this;
		}

		@Override
		public OwlimStoragePropertiesBuilder password(String password) {
			super.password(password);
			return this;
		}

		public OwlimStoragePropertiesBuilder owlimConfigFile(String owlimConfigFile) {
			this.owlimConfigFile = owlimConfigFile;
			return this;
		}

		@Override
		public OntologyStorageProperties build() {
			return new OwlimOntologyStorageProperties(this);
		}
	}
}
