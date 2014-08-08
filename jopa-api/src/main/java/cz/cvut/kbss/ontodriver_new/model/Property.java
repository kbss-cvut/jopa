package cz.cvut.kbss.ontodriver_new.model;

/**
 * Base property interface.
 * 
 * @author ledvima1
 * 
 */
public interface Property extends NamedResource {

	/**
	 * Whether this property is an object property.
	 */
	public boolean isObjectProperty();

	/**
	 * Whether this property is a data property.
	 */
	public boolean isDataProperty();

	/**
	 * Whether this property is an annotation property.
	 */
	public boolean isAnnotationProperty();

	/**
	 * Whether this property supports inferred values as well.
	 */
	public boolean isInferred();
}
