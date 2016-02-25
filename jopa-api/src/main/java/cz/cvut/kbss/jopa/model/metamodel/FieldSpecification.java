/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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