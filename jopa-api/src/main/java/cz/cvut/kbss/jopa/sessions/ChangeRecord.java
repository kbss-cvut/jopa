package cz.cvut.kbss.jopa.sessions;

/**
 * Objects of classes implementing this interface represent a change of
 * one attribute of an entity class.
 * Objects store only the new value, old value is in the original and it
 * is not needed.
 * @author kidney
 *
 */
public interface ChangeRecord {
	
	/**
	 * Returns the new value of the attribute.
	 * @return Object
	 */
	public Object getNewValue();
	
	/**
	 * Returns the name of the attribute the change is bound to.
	 * @return String
	 */
	public String getAttributeName();

}
