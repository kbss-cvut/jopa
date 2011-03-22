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
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.metamodel.ManagedType;
import cz.cvut.kbss.owlpersistence.model.metamodel.PluralAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.Type;

public class PluralAttributeImpl<X, C, E> implements PluralAttribute<X, C, E> {

	private final String name;

	private final Type<E> elementType;

	private final Class<C> collectionType;

	private final Field member;

	private final ManagedType<X> declaringType;

	private final PersistentAttributeType pat;

	private final IRI iri;

	private final CascadeType[] cascadeTypes;

	private final FetchType fetchType;

	private boolean inferred;

	private ParticipationConstraint[] constraints;

	PluralAttributeImpl(ManagedType<X> declaringType, String name, IRI iri,
			Class<C> collectionType, Type<E> elementType, Field member,
			PersistentAttributeType pat, CascadeType[] cascadeTypes,
			FetchType fetchType, boolean inferred,
			ParticipationConstraint[] constraints) {
		this.name = name;
		this.elementType = elementType;
		this.member = member;
		this.pat = pat;
		this.collectionType = collectionType;
		this.declaringType = declaringType;
		this.iri = iri;
		this.cascadeTypes = cascadeTypes;
		this.fetchType = fetchType;
		this.inferred = inferred;
		this.constraints = constraints;
	}

	
	public ManagedType<X> getDeclaringType() {
		return declaringType;
	}

	
	public Member getJavaMember() {
		return member;
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
		return true;
	}

	
	public Class<E> getBindableJavaType() {
		return elementType.getJavaType();
	}

	
	public cz.cvut.kbss.owlpersistence.model.metamodel.Bindable.BindableType getBindableType() {
		return BindableType.PLURAL_ATTRIBUTE;
	}

	
	public cz.cvut.kbss.owlpersistence.model.metamodel.PluralAttribute.CollectionType getCollectionType() {
		if (getJavaType().isAssignableFrom(List.class)) {
			return CollectionType.LIST;
		} else if (getJavaType().isAssignableFrom(Set.class)) {
			return CollectionType.SET;
		} else if (getJavaType().isAssignableFrom(Map.class)) {
			return CollectionType.MAP;
		} else if (getJavaType().isAssignableFrom(Collection.class)) {
			return CollectionType.COLLECTION;
		} else {
			throw new IllegalArgumentException();
		}
	}

	
	public Type<E> getElementType() {
		return elementType;
	}

	
	public Class<C> getJavaType() {
		return collectionType;
	}

	
	public Field getJavaField() {
		return member;
	}

	
	public IRI getIRI() {
		return iri;
	}

	
	public CascadeType[] getCascadeTypes() {
		if (getPersistentAttributeType().equals(PersistentAttributeType.OBJECT)) {
			return cascadeTypes;
		}

		return cascadeTypes;
	}

	
	public FetchType getFetchType() {
		return fetchType;
	}

	public boolean isInferred() {
		return inferred;
	}

	public ParticipationConstraint[] getConstraints() {
		return constraints;
	}
}
