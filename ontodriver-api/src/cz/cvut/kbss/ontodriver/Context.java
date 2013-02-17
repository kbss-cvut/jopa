package cz.cvut.kbss.ontodriver;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

/**
 * Represents saving context in the OntoDriver. </p>
 * 
 * The concept of a context can represent two things:
 * <ul>
 * <li>An OWL ontology module</li>
 * <li>A RDF named graph</li>
 * </ul>
 * 
 * Each context has its URI, signature and expressiveness.
 * 
 * @author kidney
 * 
 */
public class Context {

	private final URI contextUri;
	private ContextExpressiveness expressiveness;
	private Set<String> signature;

	/**
	 * 
	 * @param contextUri
	 *            URI of this context
	 * @throws NullPointerException
	 *             If the {@code contextUri} is null
	 */
	public Context(URI contextUri) {
		if (contextUri == null) {
			throw new NullPointerException("ContextUri cannot null.");
		}
		this.contextUri = contextUri;
	}

	/**
	 * Returns URI of this context.
	 * 
	 * @return URI
	 */
	public URI getUri() {
		return contextUri;
	}

	/**
	 * Returns expressiveness of this context. </p>
	 * 
	 * <i>Expressiveness</i> is a description of structure and formalism of an
	 * ontology.
	 * 
	 * @return OntologyExpressiveness
	 */
	public ContextExpressiveness getExpressiveness() {
		return expressiveness;
	}

	/**
	 * 
	 * @param expressiveness
	 *            The new expressiveness
	 * @throws NullPointerException
	 *             If the {@code expressiveness} is null
	 */
	public void setExpressiveness(ContextExpressiveness expressiveness) {
		if (expressiveness == null) {
			throw new NullPointerException("Expressiveness cannot be null.");
		}
		this.expressiveness = expressiveness;
	}

	/**
	 * Returns signature of this context. </p>
	 * 
	 * A <i>signature</i> (also known as <i>vocabulary</i>) is a set of
	 * concepts, properties and individuals contained in a given domain. </p>
	 * 
	 * The returned Set is not modifiable.
	 * 
	 * @return Signature of this context TODO THe return type may change
	 */
	public Set<String> getSignature() {
		return Collections.unmodifiableSet(signature);
	}

	/**
	 * 
	 * @param signature
	 *            The new signature
	 * @throws NullPointerException
	 *             If the {@code signature} is null
	 */
	public void setSignature(Set<String> signature) {
		if (signature == null) {
			throw new NullPointerException("Signature cannot be null.");
		}
		this.signature = signature;
	}
}
