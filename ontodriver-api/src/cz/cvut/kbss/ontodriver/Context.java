package cz.cvut.kbss.ontodriver;

import java.net.URI;
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
public interface Context {

	/**
	 * Returns URI of this context.
	 * 
	 * @return URI
	 */
	public URI getUri();

	/**
	 * Returns expressiveness of this context. </p>
	 * 
	 * <i>Expressiveness</i> is a description of structure and formalism of an
	 * ontology.
	 * 
	 * @return Expressiveness TODO The return type may change
	 */
	public String getExpressiveness();

	/**
	 * Returns signature of this context. </p>
	 * 
	 * A <i>signature</i> (also known as <i>vocabulary</i>) is a set of
	 * concepts, properties and individuals contained in a given domain.
	 * 
	 * @return Signature of this context TODO THe return type may change
	 */
	public Set<String> getSignature();
}
