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

	PluralAttributeImpl(ManagedType<X> declaringType, String name, IRI iri,
			Class<C> collectionType, Type<E> elementType, Field member,
			PersistentAttributeType pat, CascadeType[] cascadeTypes,
			FetchType fetchType) {
		this.name = name;
		this.elementType = elementType;
		this.member = member;
		this.pat = pat;
		this.collectionType = collectionType;
		this.declaringType = declaringType;
		this.iri = iri;
		this.cascadeTypes = cascadeTypes;
		this.fetchType = fetchType;
	}

	@Override
	public ManagedType<X> getDeclaringType() {
		return declaringType;
	}

	@Override
	public Member getJavaMember() {
		return member;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public cz.cvut.kbss.owlpersistence.model.metamodel.Attribute.PersistentAttributeType getPersistentAttributeType() {
		return pat;
	}

	@Override
	public boolean isAssociation() {
		return getPersistentAttributeType().equals(
				PersistentAttributeType.OBJECT);
	}

	@Override
	public boolean isCollection() {
		return true;
	}

	@Override
	public Class<E> getBindableJavaType() {
		return elementType.getJavaType();
	}

	@Override
	public cz.cvut.kbss.owlpersistence.model.metamodel.Bindable.BindableType getBindableType() {
		return BindableType.PLURAL_ATTRIBUTE;
	}

	@Override
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

	@Override
	public Type<E> getElementType() {
		return elementType;
	}

	@Override
	public Class<C> getJavaType() {
		return collectionType;
	}

	@Override
	public Field getJavaField() {
		return member;
	}

	@Override
	public IRI getIRI() {
		return iri;
	}

	@Override
	public CascadeType[] getCascadeTypes() {
		if (getPersistentAttributeType().equals(PersistentAttributeType.OBJECT)) {
			return cascadeTypes;
		}

		return cascadeTypes;
	}

	@Override
	public FetchType getFetchType() {
		return fetchType;
	}
}
