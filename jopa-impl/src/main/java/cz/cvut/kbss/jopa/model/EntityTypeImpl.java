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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.*;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class EntityTypeImpl<X> implements EntityType<X> {

    private final String name;

    private Class<X> javaType;

    private final Map<String, Attribute<? super X, ?>> attributeMap = new HashMap<>();

    private final Map<String, Attribute<X, ?>> declaredAttributeMap = new HashMap<>();

    private final IRI iri;

    private Identifier identifier;

    private TypesSpecification<X, ?> directTypes;

    private PropertiesSpecification<X, ?, ?, ?> properties;

    public EntityTypeImpl(String name, Class<X> javaType, final IRI iri) {
        this.name = name;
        this.javaType = javaType;
        this.iri = iri;
    }

    // void addAttribute(final String name, final Attribute<? super X, ?> a) {
    // attributeMap.put(name, a);
    // }

    public void addDirectTypes(TypesSpecification<X, ?> a) {
        this.directTypes = a;
    }

    public void addOtherProperties(PropertiesSpecification<X, ?, ?, ?> a) {
        this.properties = a;
    }

    public void addDeclaredAttribute(final String name, final Attribute<X, ?> a) {
        declaredAttributeMap.put(name, a);
        attributeMap.put(name, a);
    }

    public void setIdentifier(final Identifier identifier) {
        this.identifier = identifier;
    }

    @Override
    public String getName() {
        return name;
    }

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
        if (attributeMap.containsKey(name)) {
            return attributeMap.get(name);
        }
        throw new IllegalArgumentException("Attribute " + name + " is not present in type " + name);
    }

    @Override
    public Set<Attribute<? super X, ?>> getAttributes() {
        return new HashSet<>(attributeMap.values());
    }

    @Override
    public <E> CollectionAttribute<? super X, E> getCollection(String name, Class<E> elementType) {
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
        if (declaredAttributeMap.containsKey(name)) {
            return declaredAttributeMap.get(name);
        }
        throw new IllegalArgumentException("Attribute " + name + " is not declared in type " + name);
    }

    @Override
    public Set<Attribute<X, ?>> getDeclaredAttributes() {
        return new HashSet<>(declaredAttributeMap.values());
    }

    @Override
    public <E> CollectionAttribute<X, E> getDeclaredCollection(String name, Class<E> elementType) {
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
    public <E> ListAttribute<X, E> getDeclaredList(String name, Class<E> elementType) {
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
    public <K, V> MapAttribute<X, K, V> getDeclaredMap(String name, Class<K> keyType,
                                                       Class<V> valueType) {
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
        return declaredAttributeMap.values().stream().filter(Attribute::isCollection)
                                   .map(a -> (PluralAttribute<X, ?, ?>) a)
                                   .collect(Collectors.toSet());
    }

    @Override
    public <E> SetAttribute<X, E> getDeclaredSet(String name, Class<E> elementType) {
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
    public <Y> SingularAttribute<X, Y> getDeclaredSingularAttribute(String name, Class<Y> type) {
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
        return declaredAttributeMap.values().stream().filter(Attribute::isCollection).map(
                a -> (SingularAttribute<X, ?>) a)
                                   .collect(Collectors.toSet());
    }

    @Override
    public <E> ListAttribute<? super X, E> getList(String name, Class<E> elementType) {
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
    public <K, V> MapAttribute<? super X, K, V> getMap(String name, Class<K> keyType,
                                                       Class<V> valueType) {
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
        return attributeMap.values().stream().filter(Attribute::isCollection).map(
                a -> (PluralAttribute<? super X, ?, ?>) a)
                           .collect(Collectors.toSet());
    }

    @Override
    public <E> SetAttribute<? super X, E> getSet(String name, Class<E> elementType) {
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
    public <Y> SingularAttribute<? super X, Y> getSingularAttribute(String name, Class<Y> type) {
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
        return attributeMap.values().stream().filter(a -> !a.isCollection())
                           .map(a -> (SingularAttribute<? super X, ?>) a)
                           .collect(Collectors.toSet());
    }

    @Override
    public Class<X> getJavaType() {
        return javaType;
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Type.PersistenceType getPersistenceType() {
        return PersistenceType.ENTITY;
    }

    @Override
    public Class<X> getBindableJavaType() {
        return getJavaType();
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Bindable.BindableType getBindableType() {
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
    public PropertiesSpecification<? super X, ?, ?, ?> getProperties() {
        return properties;
    }

    @Override
    public FieldSpecification<? super X, ?> getFieldSpecification(String fieldName) {
        FieldSpecification<? super X, ?> att = attributeMap.get(fieldName);
        if (att == null) {
            if (directTypes != null && directTypes.getName().equals(fieldName)) {
                att = directTypes;
            } else if (properties != null && properties.getName().equals(fieldName)) {
                att = properties;
            }
        }
        if (att == null) {
            throw new IllegalArgumentException("Field " + fieldName + " is not declared in type " + name);
        }
        return att;
    }

    @Override
    public Set<FieldSpecification<? super X, ?>> getFieldSpecifications() {
        final Set<FieldSpecification<? super X, ?>> specs = new HashSet<>(attributeMap.values());
        if (directTypes != null) {
            specs.add(directTypes);
        }
        if (properties != null) {
            specs.add(properties);
        }
        return specs;
    }

    @Override
    public String toString() {
        return "EntityType{" + name + "<" + iri + ">}";
    }
}
