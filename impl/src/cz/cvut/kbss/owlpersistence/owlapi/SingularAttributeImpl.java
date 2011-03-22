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
import java.lang.reflect.Member;

import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.metamodel.ManagedType;
import cz.cvut.kbss.owlpersistence.model.metamodel.SingularAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.Type;

public class SingularAttributeImpl<X, T> implements SingularAttribute<X, T> {

	private final boolean id;

	private final String name;

	private final Type<T> type;

	private final Field m;

	private final ManagedType<X> dt;

	private final PersistentAttributeType pat;

	private final IRI iri;

	private final CascadeType[] cascadeTypes;

	private final FetchType fetchType;

	private boolean inferred;

	private ParticipationConstraint[] constraints;

	SingularAttributeImpl(ManagedType<X> declaringType, boolean id,
			String name, IRI iri, Type<T> type, Field m,
			final PersistentAttributeType pat,
			final CascadeType[] cascadeTypes, final FetchType fetchType,
			final boolean inferred, final ParticipationConstraint[] constraints) {
		this.id = id;
		this.name = name;
		this.type = type;
		this.pat = pat;
		this.m = m;
		this.dt = declaringType;
		this.iri = iri;
		this.cascadeTypes = cascadeTypes;
		this.fetchType = fetchType;
		this.inferred = inferred;
		this.constraints = constraints;
	}

	
	public Type<T> getType() {
		return type;
	}

	
	public boolean isId() {
		return id;
	}

	
	public boolean isOptional() {
		throw new UnsupportedOperationException();
	}

	
	public boolean isVersion() {
		throw new UnsupportedOperationException();
	}

	
	public ManagedType<X> getDeclaringType() {
		return dt;
	}

	
	public Member getJavaMember() {
		return m;
	}

	
	public Class<T> getJavaType() {
		return type.getJavaType();
	}

	
	public String getName() {
		return name;
	}

	
	public cz.cvut.kbss.owlpersistence.model.metamodel.Attribute.PersistentAttributeType getPersistentAttributeType() {
		return pat;
	}

	
	public boolean isAssociation() {
		return getPersistentAttributeType().equals(
				PersistentAttributeType.OBJECT);
	}

	
	public boolean isCollection() {
		return false;
	}

	
	public Class<T> getBindableJavaType() {
		return type.getJavaType();
	}

	
	public cz.cvut.kbss.owlpersistence.model.metamodel.Bindable.BindableType getBindableType() {
		return BindableType.SINGULAR_ATTRIBUTE;
	}

	
	public Field getJavaField() {
		return m;
	}

	
	public IRI getIRI() {
		return iri;
	}

	
	public CascadeType[] getCascadeTypes() {
		return cascadeTypes;
	}

	
	public FetchType getFetchType() {
		return fetchType;
	}

	
	public String toString() {
		return "SingularAttribute[" + name + "]";
	}

	public void setInferred(boolean inferred) {
		this.inferred = inferred;
	}

	public boolean isInferred() {
		return inferred;
	}

	public ParticipationConstraint[] getConstraints() {
		return constraints;
	}
}
