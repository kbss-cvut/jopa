/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;

import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.metamodel.ManagedType;
import cz.cvut.kbss.owlpersistence.model.metamodel.TypesSpecification;

public class TypesSpecificationImpl<X, Y> implements
		TypesSpecification<X, Y> {
	private final ManagedType<X> declaringType;
	private final FetchType fetchType;
	private final Field javaField;
	private Class<Y> javaType;
	private boolean inferred;

	public TypesSpecificationImpl(final ManagedType<X> declaringType,
			final FetchType fetchType, final Field javaField,
			final Class<Y> javaType, boolean inferred) {

		this.declaringType = declaringType;
		this.fetchType = fetchType;
		this.javaField = javaField;
		this.javaType = javaType;
		this.inferred = inferred;
	}

	
	public ManagedType<X> getDeclaringType() {
		return declaringType;
	}

	
	public FetchType getFetchType() {
		return fetchType;
	}

	
	public Field getJavaField() {
		return javaField;
	}

	
	public Class<Y> getJavaType() {
		return javaType;
	}

	public boolean isInferred() {
		return inferred;
	}

}
