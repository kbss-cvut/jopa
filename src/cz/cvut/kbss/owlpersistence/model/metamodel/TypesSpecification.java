package cz.cvut.kbss.owlpersistence.model.metamodel;

import cz.cvut.kbss.owlpersistence.NonJPA;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;

/**
 * Instances of the type SetAttribute represent persistent java.util.Set-valued
 * attributes.
 * 
 * @param <X>
 *            The type the represented Set belongs to
 * @param <E>
 *            The element type of the represented Set
 */
public interface TypesSpecification<X, E> {

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
	Class<E> getJavaType();

	/**
	 * Return the java.lang.reflect.Member for the represented attribute.
	 * 
	 * @return corresponding java.lang.reflect.Member
	 */
	@NonJPA
	java.lang.reflect.Field getJavaField();

	/**
	 * Return the fetch type for this attribute
	 */
	@NonJPA
	FetchType getFetchType();
}
