package cz.cvut.kbss.ontodriver_new.model;

/**
 * Represents either an OWL axiom or a RDF triple. </p>
 * 
 * 
 * @author ledvima1
 * 
 */
public interface Axiom {

	/**
	 * Gets the subject of this axiom.
	 * 
	 * @return Subject named resource
	 */
	public NamedResource getSubject();

	/**
	 * Gets the assertion of this axiom.
	 * 
	 * @return Assertion named resource
	 */
	public Assertion getAssertion();

	/**
	 * Gets the value of this axiom.
	 * 
	 * @return Value, either a NamedResource, or a literal value
	 */
	public <T> Value<T> getValue();
}
