package cz.cvut.kbss.ontodriver;

import java.net.URI;

public final class SesameOntologyStorageProperties extends OntologyStorageProperties {

	private final String repositoryId;

	public SesameOntologyStorageProperties(URI ontologyUri, URI physicalUri, String repositoryId,
			OntologyConnectorType connectorType) {
		super(ontologyUri, physicalUri, connectorType);
		this.repositoryId = repositoryId;
	}

	public SesameOntologyStorageProperties(URI ontologyUri, URI physicalUri, String repositoryId,
			OntologyConnectorType connectorType, String username, String password) {
		super(ontologyUri, physicalUri, connectorType, username, password);
		this.repositoryId = repositoryId;
	}

	public SesameOntologyStorageProperties(SesameOntologyStoragePropertiesBuilder builder) {
		super(builder);
		this.repositoryId = builder.repoId;
	}

	public String getRepositoryId() {
		return repositoryId;
	}

	public static SesameOntologyStoragePropertiesBuilder repositoryId(String repositoryId) {
		return new SesameOntologyStoragePropertiesBuilder().repositoryId(repositoryId);
	}

	public static final class SesameOntologyStoragePropertiesBuilder extends
			OntologyStoragePropertiesBuilder {
		private String repoId;

		public SesameOntologyStoragePropertiesBuilder repositoryId(String repositoryId) {
			this.repoId = repositoryId;
			return this;
		}
	}
}
