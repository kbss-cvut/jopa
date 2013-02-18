package cz.cvut.kbss.ontodriver;

import java.net.URI;

/**
 * Holds properties of an ontology storage. </p>
 * 
 * These properties can be used to create a DataSource representing the storage.
 * 
 * @author kidney
 * 
 */
public final class OntologyStorageProperties {

	/** URI of the ontology */
	private final URI ontologyUri;
	/** URI of the physical storage, e. g. OWLDB database, OWLIM storage, file */
	private final URI physicalUri;
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
	 * @throws NullPointerException
	 *             If {@code ontologyUri} or {@code physicalUri} is null
	 */
	public OntologyStorageProperties(URI ontologyUri, URI physicalUri) {
		super();
		if (ontologyUri == null) {
			throw new NullPointerException("OntologyURI cannot be null.");
		}
		if (physicalUri == null) {
			throw new NullPointerException("PhysicalURI cannot be null.");
		}
		this.ontologyUri = ontologyUri;
		this.physicalUri = physicalUri;
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
	 * @param username
	 *            Username for the storage. Optional
	 * @param password
	 *            Password for the storage. Optional
	 * @throws NullPointerException
	 *             If {@code ontologyUri} or {@code physicalUri} is null
	 * @see #OntologyStorageProperties(URI, URI)
	 */
	public OntologyStorageProperties(URI ontologyUri, URI physicalUri, String username,
			String password) {
		super();
		if (ontologyUri == null) {
			throw new NullPointerException("OntologyURI cannot be null.");
		}
		if (physicalUri == null) {
			throw new NullPointerException("PhysicalURI cannot be null.");
		}
		this.ontologyUri = ontologyUri;
		this.physicalUri = physicalUri;
		this.username = username;
		this.password = password;
	}

	public URI getOntologyURI() {
		return ontologyUri;
	}

	public URI getPhysicalURI() {
		return physicalUri;
	}

	public String getUsername() {
		return username;
	}

	public String getPassword() {
		return password;
	}
}
