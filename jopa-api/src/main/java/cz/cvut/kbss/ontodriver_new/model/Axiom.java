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
	 * @return Subject resource
	 */
	public NamedResource getSubject();

	/**
	 * Gets the property of this axiom.
	 * 
	 * @return Property resource
	 */
	public NamedResource getProperty();

	/**
	 * Gets the value of this axiom.
	 * 
	 * @return Value, either a NamedResource, or a literal value
	 */
	public <T> Value<T> getValue();
}
