package cz.cvut.kbss.owlpersistence.model.metamodel;

import cz.cvut.kbss.owlpersistence.NonJPA;
import cz.cvut.kbss.owlpersistence.UnusedJPA;
import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;

/**
 * Represents an attribute of a Java type.
 * 
 * @param <X>
 *            The represented type that contains the attribute
 * @param <Y>
 *            The type of the represented attribute
 */
public interface Attribute<X, Y> {
	public static enum PersistentAttributeType {
		// @UnusedJPA
		// @Deprecated
		// MANY_TO_ONE,
		//
		// @UnusedJPA
		// @Deprecated
		// ONE_TO_ONE,
		//
		// /**
		// * correspond to the datatype properties
		// */
		// @UnusedJPA
		// @Deprecated
		// BASIC,

		/**
		 * correspond to the object properties
		 */
		@NonJPA
		DATA,

		/**
		 * correspond to the object properties
		 */
		@NonJPA
		OBJECT,

		/**
		 * correspond to the object properties
		 */
		@NonJPA
		ANNOTATION,

		// @UnusedJPA
		// @Deprecated
		// EMBEDDED,
		//
		// @UnusedJPA
		// @Deprecated
		// MANY_TO_MANY,
		//
		// @UnusedJPA
		// @Deprecated
		// ONE_TO_MANY,
		//
		// @UnusedJPA
		// @Deprecated
		// ELEMENT_COLLECTION
	}

	/**
	 * Return the name of the attribute.
	 * 
	 * @return name
	 */
	String getName();

	/**
	 * Return the persistent attribute type for the attribute.
	 * 
	 * @return persistent attribute type
	 */
	PersistentAttributeType getPersistentAttributeType();

	/**
	 * Return the managed type representing the type in which the attribute was
	 * declared.
	 * 
	 * @return declaring type
	 */
	ManagedType<X> getDeclaringType();

	/**
	 * Return the Java type of the represented attribute.
	 * 
	 * @return Java type
	 */
	Class<Y> getJavaType();

	/**
	 * Return the java.lang.reflect.Member for the represented attribute.
	 * 
	 * @return corresponding java.lang.reflect.Member
	 */
	@UnusedJPA
	@Deprecated
	java.lang.reflect.Member getJavaMember();

	/**
	 * Return the java.lang.reflect.Member for the represented attribute.
	 * 
	 * @return corresponding java.lang.reflect.Member
	 */
	@NonJPA
	java.lang.reflect.Field getJavaField();

	/**
	 * Return the java.lang.reflect.Member for the represented attribute.
	 * 
	 * @return corresponding java.lang.reflect.Member
	 */
	@NonJPA
	IRI getIRI();

	/**
	 * Return the set of cascadetypes specified for this attribute.
	 * 
	 * @return corresponding array of cascade types. This method returns an
	 *         empty array if no cascade type is specified.
	 * @throws IllegalArgumentException
	 *             if getPersistentAttributeType() returns DATA or ANNOTATION.
	 */
	@NonJPA
	CascadeType[] getCascadeTypes();

	/**
	 * Return the fetch type for this attribute
	 */
	@NonJPA
	FetchType getFetchType();

	/**
	 * Is the attribute an association.
	 * 
	 * @return boolean indicating whether the attribute corresponds
	 * 
	 *         to an association
	 */
	boolean isAssociation();

	/**
	 * Is the attribute collection-valued (represents a Collection, Set, List,
	 * or Map).
	 * 
	 * @return boolean indicating whether the attribute is
	 * 
	 *         collection-valued
	 */
	boolean isCollection();
}
