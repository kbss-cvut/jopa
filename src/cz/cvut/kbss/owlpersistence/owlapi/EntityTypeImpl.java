package cz.cvut.kbss.owlpersistence.owlapi;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.CollectionAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.TypesSpecification;
import cz.cvut.kbss.owlpersistence.model.metamodel.EntityType;
import cz.cvut.kbss.owlpersistence.model.metamodel.IdentifiableType;
import cz.cvut.kbss.owlpersistence.model.metamodel.Identifier;
import cz.cvut.kbss.owlpersistence.model.metamodel.ListAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.MapAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.PluralAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.owlpersistence.model.metamodel.SetAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.SingularAttribute;

public class EntityTypeImpl<X> implements EntityType<X> {

	private final String name;

	private Class<X> javaType;

	private final Map<String, Attribute<? super X, ?>> attributeMap = new HashMap<String, Attribute<? super X, ?>>();

	private final Map<String, Attribute<X, ?>> declaredAttributeMap = new HashMap<String, Attribute<X, ?>>();

	private final IRI iri;

	private Identifier identifier;

	private TypesSpecification<X, ?> directTypes;

	private PropertiesSpecification<X, ?> properties;

	public EntityTypeImpl(String name, Class<X> javaType, final IRI iri) {
		this.name = name;
		this.javaType = javaType;
		this.iri = iri;
	}

	// void addAttribute(final String name, final Attribute<? super X, ?> a) {
	// attributeMap.put(name, a);
	// }

	void addDirectTypes(TypesSpecification<X, ?> a) {
		this.directTypes = a;
	}

	void addOtherProperties(PropertiesSpecification<X, ?> a) {
		this.properties = a;
	}

	void addDeclaredAttribute(final String name, final Attribute<X, ?> a) {
		declaredAttributeMap.put(name, a);
		attributeMap.put(name, a);
	}

	void setIdentifier(final Identifier identifier) {
		this.identifier = identifier;
	}

	@Override
	public String getName() {
		return name;
	}

	// @Override
	// public <Y> SingularAttribute<X, Y> getDeclaredId(Class<Y> type) {
	// for (final Attribute<X, ?> a : declaredAttributeMap.values()) {
	// final SingularAttribute<X, Y> sa = (SingularAttribute<X, Y>) a;
	//
	// if (sa.isId() && sa.getJavaType().isAssignableFrom(type)) {
	// return sa;
	// }
	// }
	//
	// throw new IllegalArgumentException();
	// }
	//
	// @Override
	// public <Y> SingularAttribute<? super X, Y> getId(Class<Y> type) {
	// for (final Attribute<? super X, ?> a : attributeMap.values()) {
	// final SingularAttribute<? super X, Y> sa = (SingularAttribute<? super X,
	// Y>) a;
	//
	// if (sa.isId() && sa.getJavaType().isAssignableFrom(type)) {
	// return sa;
	// }
	// }
	//
	// throw new IllegalArgumentException();
	// }
	//
	// @Override
	// public Type<?> getIdType() {
	// return idField.getType();
	// }

	@Override
	public <Y> SingularAttribute<X, Y> getDeclaredVersion(Class<Y> type) {
		// TODO
		throw new UnsupportedOperationException();
	}

	@Override
	public Set<SingularAttribute<? super X, ?>> getIdClassAttributes() {
		// TODO
		throw new UnsupportedOperationException();
	}

	@Override
	public IdentifiableType<? super X> getSupertype() {
		// TODO
		throw new UnsupportedOperationException();
	}

	@Override
	public <Y> SingularAttribute<? super X, Y> getVersion(Class<Y> type) {
		// TODO
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean hasSingleIdAttribute() {
		// TODO
		return false;
	}

	@Override
	public boolean hasVersionAttribute() {
		// TODO
		return false;
	}

	@Override
	public Attribute<? super X, ?> getAttribute(String name) {
		return attributeMap.get(name);
	}

	@Override
	public Set<Attribute<? super X, ?>> getAttributes() {
		return new HashSet<Attribute<? super X, ?>>(attributeMap.values());
	}

	@Override
	public <E> CollectionAttribute<? super X, E> getCollection(String name,
			Class<E> elementType) {
		final CollectionAttribute<? super X, E> a = (CollectionAttribute<? super X, E>) getAttribute(name);

		if (!a.getBindableJavaType().isAssignableFrom(elementType)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public CollectionAttribute<? super X, ?> getCollection(String name) {
		return getCollection(name, Object.class);
	}

	@Override
	public Attribute<X, ?> getDeclaredAttribute(String name) {
		return declaredAttributeMap.get(name);
	}

	@Override
	public Set<Attribute<X, ?>> getDeclaredAttributes() {
		return new HashSet<Attribute<X, ?>>(declaredAttributeMap.values());
	}

	@Override
	public <E> CollectionAttribute<X, E> getDeclaredCollection(String name,
			Class<E> elementType) {
		final CollectionAttribute<X, E> a = (CollectionAttribute<X, E>) getDeclaredAttribute(name);

		if (!a.getBindableJavaType().isAssignableFrom(elementType)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public CollectionAttribute<X, ?> getDeclaredCollection(String name) {
		return getDeclaredCollection(name, Object.class);
	}

	@Override
	public <E> ListAttribute<X, E> getDeclaredList(String name,
			Class<E> elementType) {
		final ListAttribute<X, E> a = (ListAttribute<X, E>) getDeclaredAttribute(name);

		if (!a.getBindableJavaType().isAssignableFrom(elementType)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public ListAttribute<X, ?> getDeclaredList(String name) {
		return getDeclaredList(name, Object.class);
	}

	@Override
	public <K, V> MapAttribute<X, K, V> getDeclaredMap(String name,
			Class<K> keyType, Class<V> valueType) {
		final MapAttribute<X, K, V> a = (MapAttribute<X, K, V>) getDeclaredAttribute(name);

		if (!a.getKeyJavaType().isAssignableFrom(keyType)
				|| !a.getBindableJavaType().isAssignableFrom(valueType)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public MapAttribute<X, ?, ?> getDeclaredMap(String name) {
		return getDeclaredMap(name, Object.class, Object.class);
	}

	@Override
	public Set<PluralAttribute<X, ?, ?>> getDeclaredPluralAttributes() {
		final Set<PluralAttribute<X, ?, ?>> set = new HashSet<PluralAttribute<X, ?, ?>>();

		for (final Attribute<X, ?> a : declaredAttributeMap.values()) {
			if (a.isCollection()) {
				set.add((PluralAttribute<X, ?, ?>) a);
			}
		}

		return set;
	}

	@Override
	public <E> SetAttribute<X, E> getDeclaredSet(String name,
			Class<E> elementType) {
		final SetAttribute<X, E> a = (SetAttribute<X, E>) getDeclaredAttribute(name);

		if (!a.getBindableJavaType().isAssignableFrom(elementType)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public SetAttribute<X, ?> getDeclaredSet(String name) {
		return getDeclaredSet(name, Object.class);
	}

	@Override
	public <Y> SingularAttribute<X, Y> getDeclaredSingularAttribute(
			String name, Class<Y> type) {
		final SingularAttribute<X, Y> a = (SingularAttribute<X, Y>) getDeclaredAttribute(name);

		if (!a.getJavaType().isAssignableFrom(type)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public SingularAttribute<X, ?> getDeclaredSingularAttribute(String name) {
		return getDeclaredSingularAttribute(name, Object.class);
	}

	@Override
	public Set<SingularAttribute<X, ?>> getDeclaredSingularAttributes() {
		final Set<SingularAttribute<X, ?>> set = new HashSet<SingularAttribute<X, ?>>();

		for (final Attribute<X, ?> a : declaredAttributeMap.values()) {
			if (!a.isCollection()) {
				set.add((SingularAttribute<X, ?>) a);
			}
		}

		return set;
	}

	@Override
	public <E> ListAttribute<? super X, E> getList(String name,
			Class<E> elementType) {
		final ListAttribute<? super X, E> a = (ListAttribute<? super X, E>) getAttribute(name);

		if (!a.getBindableJavaType().isAssignableFrom(elementType)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public ListAttribute<? super X, ?> getList(String name) {
		return getList(name, Object.class);
	}

	@Override
	public <K, V> MapAttribute<? super X, K, V> getMap(String name,
			Class<K> keyType, Class<V> valueType) {
		final MapAttribute<? super X, K, V> a = (MapAttribute<? super X, K, V>) getAttribute(name);

		if (!a.getKeyJavaType().isAssignableFrom(keyType)
				|| !a.getBindableJavaType().isAssignableFrom(valueType)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public MapAttribute<? super X, ?, ?> getMap(String name) {
		return getMap(name, Object.class, Object.class);
	}

	@Override
	public Set<PluralAttribute<? super X, ?, ?>> getPluralAttributes() {
		final Set<PluralAttribute<? super X, ?, ?>> set = new HashSet<PluralAttribute<? super X, ?, ?>>();

		for (final Attribute<? super X, ?> a : attributeMap.values()) {
			if (a.isCollection()) {
				set.add((PluralAttribute<? super X, ?, ?>) a);
			}
		}

		return set;
	}

	@Override
	public <E> SetAttribute<? super X, E> getSet(String name,
			Class<E> elementType) {
		final SetAttribute<? super X, E> a = (SetAttribute<? super X, E>) getAttribute(name);

		if (!a.getBindableJavaType().isAssignableFrom(elementType)) {
			throw new IllegalArgumentException();
		}

		return a;
	}

	@Override
	public SetAttribute<? super X, ?> getSet(String name) {
		return getSet(name, Object.class);
	}

	@Override
	public <Y> SingularAttribute<? super X, Y> getSingularAttribute(
			String name, Class<Y> type) {
		final Attribute<? super X, ?> a = getAttribute(name);

		if (!a.getJavaType().isAssignableFrom(type)) {
			throw new IllegalArgumentException();
		}

		return (SingularAttribute<? super X, Y>) a;
	}

	@Override
	public SingularAttribute<? super X, ?> getSingularAttribute(String name) {
		return getSingularAttribute(name, Object.class);
	}

	@Override
	public Set<SingularAttribute<? super X, ?>> getSingularAttributes() {
		final Set<SingularAttribute<? super X, ?>> set = new HashSet<SingularAttribute<? super X, ?>>();

		for (final Attribute<? super X, ?> a : attributeMap.values()) {
			if (!a.isCollection()) {
				set.add((SingularAttribute<? super X, ?>) a);
			}
		}

		return set;
	}

	@Override
	public Class<X> getJavaType() {
		return javaType;
	}

	@Override
	public cz.cvut.kbss.owlpersistence.model.metamodel.Type.PersistenceType getPersistenceType() {
		return PersistenceType.ENTITY;
	}

	@Override
	public Class<X> getBindableJavaType() {
		return getJavaType();
	}

	@Override
	public cz.cvut.kbss.owlpersistence.model.metamodel.Bindable.BindableType getBindableType() {
		return BindableType.ENTITY_TYPE;
	}

	@Override
	public IRI getIRI() {
		return iri;
	}

	@Override
	public Identifier getIdentifier() {
		return identifier;
	}

	@Override
	public TypesSpecification<? super X, ?> getTypes() {
		return directTypes;
	}

	@Override
	public PropertiesSpecification<? super X, ?> getProperties() {
		return properties;
	}
}
