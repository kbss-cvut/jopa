package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.model.annotations.FetchType;

public interface FieldSpecification<X, E> {

	/**
	 * Return the managed type representing the type in which the attribute was
	 * declared.
	 * 
	 * @return declaring type
	 */
	public ManagedType<X> getDeclaringType();

	/**
	 * Return the Java type of the represented attribute.
	 * 
	 * @return Java type
	 */
	public Class<E> getJavaType();

	/**
	 * Return the java.lang.reflect.Member for the represented attribute.
	 * 
	 * @return corresponding java.lang.reflect.Member
	 */
	@NonJPA
	public java.lang.reflect.Field getJavaField();

	/**
	 * Return the fetch type for this attribute
	 */
	@NonJPA
	public FetchType getFetchType();

	public boolean isInferred();

	public boolean includeExplicit();

	public  boolean isReadOnly();

	/**
	 * Return the name of the attribute.
	 *
	 * @return name
	 */
	String getName();
}