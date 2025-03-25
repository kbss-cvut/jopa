/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.AbstractTypeException;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Instances of the type AbstractIdentifiableType represent entity or mapped superclass types which can be queried for
 * attributes, subtypes and so on.
 * <p>
 * After all attributes and fields are initialized, {@link #finish()} should be invoked to ensure consistent behavior
 * and initialization of default values of attributes that were not provided during building.
 *
 * @param <X> Entity type being represented by this instance
 */

public abstract class AbstractIdentifiableType<X> implements IdentifiableType<X> {

    private final Class<X> javaType;

    private Identifier<X, ?> identifier;

    private Set<AbstractIdentifiableType<? super X>> supertypes = Set.of();

    /// AIT which is superclass of this AIT, and from which we inherit attributes
    private AbstractIdentifiableType<? super X> classSupertype;

    private Set<AbstractIdentifiableType<? extends X>> subtypes;

    private TypesSpecification<X, ?> directTypes;

    private PropertiesSpecification<X, ?, ?, ?> properties;

    private final Map<String, AbstractAttribute<X, ?>> declaredAttributes = new HashMap<>();

    /**
     * For each attribute name, have a map of subtypes with the actual type parameter and a version of the attribute
     * corresponding to this subtype.
     */
    private Map<String, Map<Class<? extends X>, AbstractAttribute<X, ?>>> declaredGenericAttributes;

    private Map<String, AbstractQueryAttribute<X, ?>> declaredQueryAttributes;

    /**
     * For each query attribute name, have a map of subtypes with the actual type parameter and a version of the
     * attribute corresponding to this subtype.
     */
    private Map<String, Map<Class<? extends X>, AbstractQueryAttribute<X, ?>>> declaredGenericQueryAttributes;

    private EntityLifecycleListenerManager lifecycleListenerManager = EntityLifecycleListenerManager.empty();

    AbstractIdentifiableType(Class<X> javaType) {
        this.javaType = javaType;
    }

    void addDeclaredAttribute(String name, AbstractAttribute<X, ?> a) {
        declaredAttributes.put(name, a);
    }

    void addDeclaredGenericAttribute(String name, Class<? extends X> subtype, final AbstractAttribute<X, ?> a) {
        if (declaredGenericAttributes == null) {
            this.declaredGenericAttributes = new HashMap<>();
        }
        final Map<Class<? extends X>, AbstractAttribute<X, ?>> map = declaredGenericAttributes.computeIfAbsent(name, k -> new HashMap<>(subtypes.size()));
        map.put(subtype, a);
    }

    void addDeclaredQueryAttribute(String name, AbstractQueryAttribute<X, ?> a) {
        if (declaredQueryAttributes == null) {
            this.declaredQueryAttributes = new HashMap<>();
        }
        declaredQueryAttributes.put(name, a);
    }

    void addDeclaredGenericQueryAttribute(String name, Class<? extends X> subtype, AbstractQueryAttribute<X, ?> a) {
        if (declaredGenericQueryAttributes == null) {
            this.declaredGenericQueryAttributes = new HashMap<>();
        }
        final Map<Class<? extends X>, AbstractQueryAttribute<X, ?>> map = declaredGenericQueryAttributes.computeIfAbsent(name, k -> new HashMap<>(subtypes.size()));
        map.put(subtype, a);
    }

    /**
     * Set supertypes of this instance, and for all given supertypes add this as their subtype. This method should not
     * be called multiple times on one instance.
     *
     * @param supertypes Supertypes to set
     */
    void setSupertypes(Set<AbstractIdentifiableType<? super X>> supertypes) {
        assert supertypes != null;
        this.supertypes = supertypes;

        supertypes.forEach(supertype -> supertype.addSubtype(this));
        /// find non-abstract parent (class), and use it later for finding attributes, as attributes can be only in AITs that represent classes
        supertypes.stream().filter(ait -> !ait.getJavaType().isInterface()).findAny()
                  .ifPresent(clsSupertype -> this.classSupertype = clsSupertype);
    }

    private void addSubtype(AbstractIdentifiableType<? extends X> subtype) {
        if (subtypes == null) {
            this.subtypes = new HashSet<>(2);
        }
        subtypes.add(subtype);
    }

    void addDirectTypes(TypesSpecification<X, ?> a) {
        this.directTypes = a;
    }

    void addOtherProperties(PropertiesSpecification<X, ?, ?, ?> a) {
        this.properties = a;
    }

    void setIdentifier(final Identifier<X, ?> identifier) {
        this.identifier = identifier;
    }

    /**
     * Finishes initialization of this AIT, setting default values to selected fields and making them immutable.
     */
    void finish() {
        this.subtypes = subtypes == null ? Set.of() : Collections.unmodifiableSet(subtypes);
        this.declaredGenericAttributes = declaredGenericAttributes == null ? Map.of() : Collections.unmodifiableMap(declaredGenericAttributes);
        this.declaredQueryAttributes = declaredQueryAttributes == null ? Map.of() : Collections.unmodifiableMap(declaredQueryAttributes);
        this.declaredGenericQueryAttributes = declaredGenericQueryAttributes == null ? Map.of() : Collections.unmodifiableMap(declaredGenericQueryAttributes);
    }

    @Override
    public boolean hasSingleIdAttribute() {
        return true;    // We do not support id classes
    }

    @Override
    public <Y> SingularAttribute<X, Y> getDeclaredVersion(Class<Y> type) {
        // TODO
        throw new UnsupportedOperationException();
    }

    @Override
    public Set<AbstractIdentifiableType<? super X>> getSupertypes() {
        return supertypes;
    }

    /**
     * Whether this managed type has any managed subtypes (entities or mapped superclasses).
     *
     * @return {@code true} when managed subtypes exist, {@code false} otherwise
     */
    public boolean hasSubtypes() {
        return !subtypes.isEmpty();
    }

    /**
     * Whether the Java type represented by this type is an abstract class.
     *
     * @return {@code true} if the represented Java type is abstract, {@code false} otherwise
     */
    public abstract boolean isAbstract();

    /**
     * Gets the Java type represented by the metamodel instance that can be instantiated.
     * <p>
     * The purpose of this method is mainly to return the generated subclass of {@link #getJavaType()} that is used for
     * instantiation.
     *
     * @return Instantiable Java type
     */
    public Class<? extends X> getInstantiableJavaType() {
        throw new AbstractTypeException("Type " + getJavaType() + " is an abstract type and cannot be instantiated!");
    }

    public Set<AbstractIdentifiableType<? extends X>> getSubtypes() {
        return subtypes;
    }

    @Override
    public <Y> SingularAttribute<? super X, Y> getVersion(Class<Y> type) {
        // TODO
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean hasVersionAttribute() {
        return false;
    }

    @Override
    public Set<Attribute<? super X, ?>> getAttributes() {
        final Set<Attribute<? super X, ?>> attributes = (Set) getDeclaredAttributesImpl();
        if (classSupertype != null) {
            attributes.addAll(classSupertype.getAttributes(javaType));
        }
        return attributes;
    }

    /**
     * Gets declared attributes, including generic ones.
     * <p>
     * For generic attributes the first available actually typed element is used.
     * <p>
     * This method is expected to be called if this instance represents an abstract type, not an instantiable entity
     * class.
     */
    private Set<Attribute<X, ?>> getDeclaredAttributesImpl() {
        final Set<Attribute<X, ?>> attributes = new HashSet<>(declaredAttributes.values());
        declaredGenericAttributes.values().stream().map(m -> m.values().iterator().next()).forEach(attributes::add);
        return attributes;
    }

    /**
     * Gets attributes for the specified subtype.
     * <p>
     * The subtype is used to determine which versions of generic attributes should be included in the result.
     *
     * @param subtype Type for which attributes should be returned
     * @return Set of all attributes for this entity type
     */
    Set<Attribute<? super X, ?>> getAttributes(Class<? extends X> subtype) {
        final Set<Attribute<? super X, ?>> attributes = new HashSet<>(declaredAttributes.values());
        declaredGenericAttributes.values().stream()
                                 .flatMap(m -> m.entrySet().stream().filter(e -> e.getKey().equals(subtype))
                                                .map(Map.Entry::getValue)).forEach(attributes::add);
        if (classSupertype != null) {
            attributes.addAll(classSupertype.getAttributes(subtype));
        }
        return attributes;
    }

    @Override
    public Set<QueryAttribute<? super X, ?>> getQueryAttributes() {
        final Set<QueryAttribute<? super X, ?>> queryAttributes = new HashSet<>(declaredQueryAttributes.values());
        declaredGenericQueryAttributes.values().stream().map(m -> m.values().iterator().next())
                                      .forEach(queryAttributes::add);

        if (classSupertype != null) {
            queryAttributes.addAll(classSupertype.getQueryAttributes(javaType));
        }
        return queryAttributes;
    }

    /**
     * Gets query attributes for the specified subtype.
     * <p>
     * The subtype is used to determine which versions of generic query attributes should be included in the result.
     *
     * @param subtype Type for which attributes should be returned
     * @return Set of all query attributes for this entity type
     */
    Set<QueryAttribute<? super X, ?>> getQueryAttributes(Class<?> subtype) {
        final Set<QueryAttribute<? super X, ?>> queryAttributes = new HashSet<>(declaredQueryAttributes.values());
        declaredGenericQueryAttributes.values().stream().map(m -> m.get(subtype)).forEach(queryAttributes::add);
        if (classSupertype != null) {
            queryAttributes.addAll(classSupertype.getQueryAttributes(subtype));
        }
        return queryAttributes;
    }

    @Override
    public AbstractAttribute<? super X, ?> getAttribute(String name) {
        Objects.requireNonNull(name);
        return getDeclaredAttributeImpl(name).orElseGet(() -> {
            if (classSupertype != null) {
                return classSupertype.getAttribute(name, javaType);
            }
            throw attributeMissing(name, false);
        });
    }

    /**
     * Gets attribute with the specified name.
     * <p>
     * The specified subtype is used in case the name represents a generic attribute, in which case a version for the
     * specified subtype is resolved.
     *
     * @param name    Attribute name
     * @param subtype Type for which a possible generic attribute is resolved
     * @return Attribute with matching name
     */
    AbstractAttribute<? super X, ?> getAttribute(String name, Class<? extends X> subtype) {
        return getDeclaredAttribute(name, subtype).orElseGet(() -> {
            if (classSupertype != null) {
                return classSupertype.getAttribute(name, subtype);
            }
            throw attributeMissing(name, false);
        });
    }

    private Optional<AbstractAttribute<? super X, ?>> getDeclaredAttribute(String name, Class<? extends X> subtype) {
        if (declaredAttributes.containsKey(name)) {
            return Optional.of(declaredAttributes.get(name));
        }
        if (declaredGenericAttributes.containsKey(name)) {
            final Map<Class<? extends X>, AbstractAttribute<X, ?>> map = declaredGenericAttributes.get(name);
            if (map.containsKey(subtype)) {
                return Optional.of(map.get(subtype));
            }
        }
        return Optional.empty();
    }

    /**
     * Gets a declared attribute with the specified name.
     * <p>
     * If the name represents a generic attribute, the first available version is returned.
     * <p>
     * This method is expected to be called when this instance represents an abstract class, not an instantiable entity
     * class.
     *
     * @param name Attribute name
     * @return Matching attribute
     */
    private Optional<AbstractAttribute<? super X, ?>> getDeclaredAttributeImpl(String name) {
        if (declaredAttributes.containsKey(name)) {
            return Optional.of(declaredAttributes.get(name));
        }
        if (declaredGenericAttributes.containsKey(name)) {
            return Optional.of(declaredGenericAttributes.get(name).values().iterator().next());
        }
        return Optional.empty();
    }

    protected IllegalArgumentException attributeMissing(String name, boolean declared) {
        return new IllegalArgumentException("Attribute " + name + " is not " + (declared ? "declared" : "present") + " in type " + this);
    }

    @Override
    public boolean hasQueryAttribute(String name) {
        Objects.requireNonNull(name);
        if (declaredQueryAttributes.containsKey(name) || declaredGenericQueryAttributes.containsKey(name)) {
            return true;
        }

        if (classSupertype != null) {
            return classSupertype.hasQueryAttribute(name);
        }
        return false;
    }

    @Override
    public AbstractQueryAttribute<? super X, ?> getQueryAttribute(String name) {
        Objects.requireNonNull(name);
        if (declaredQueryAttributes.containsKey(name)) {
            return declaredQueryAttributes.get(name);
        }
        if (declaredGenericQueryAttributes.containsKey(name)) {
            return declaredGenericQueryAttributes.get(name).values().iterator().next();
        }
        if (classSupertype != null) {
            return classSupertype.getQueryAttribute(name, javaType);
        }

        throw attributeMissing(name, false);
    }

    AbstractQueryAttribute<? super X, ?> getQueryAttribute(String name, Class<? extends X> subtype) {
        if (declaredQueryAttributes.containsKey(name)) {
            return declaredQueryAttributes.get(name);
        }
        if (declaredGenericQueryAttributes.containsKey(name)) {
            final Map<Class<? extends X>, AbstractQueryAttribute<X, ?>> map = declaredGenericQueryAttributes.get(name);
            if (map.containsKey(subtype)) {
                return map.get(subtype);
            }
        }
        if (classSupertype != null) {
            return classSupertype.getQueryAttribute(name, subtype);
        }
        throw attributeMissing(name, false);
    }

    @Override
    public CollectionAttribute<? super X, ?> getCollection(String name) {
        return getCollection(name, Object.class);
    }

    @Override
    public <E> CollectionAttribute<? super X, E> getCollection(String name, Class<E> elementType) {
        return getPluralAttribute("Collection", name, elementType, CollectionAttribute.class);
    }

    private <E, R extends PluralAttribute<? super X, ?, E>> R getPluralAttribute(String type, String name,
                                                                                 Class<E> elementType,
                                                                                 Class<R> attType) {
        final Attribute<? super X, ?> a = getAttribute(name);

        checkPluralAttribute(a, type, name, elementType, attType, false);
        return attType.cast(a);
    }

    private <E, R extends PluralAttribute<? super X, ?, E>> void checkPluralAttribute(Attribute<? super X, ?> att,
                                                                                      String type, String name,
                                                                                      Class<E> elementType,
                                                                                      Class<R> attType,
                                                                                      boolean declared) {
        if (!attType.isAssignableFrom(att.getClass())) {
            throw pluralAttNotFound(type, name, elementType, declared);
        }

        final PluralAttribute<? super X, ?, E> colAtt = (PluralAttribute<? super X, ?, E>) att;
        if (!elementType.isAssignableFrom(colAtt.getBindableJavaType())) {
            throw pluralAttNotFound(type, name, elementType, declared);
        }
    }

    private IllegalArgumentException pluralAttNotFound(String type, String name, Class<?> elementType,
                                                       boolean declared) {
        return new IllegalArgumentException(type + " attribute " + name + " with element type " + elementType + " is not " + (declared ? "declared" : "present") + " in type " + this);
    }

    @Override
    public <E> ListAttribute<? super X, E> getList(String name, Class<E> elementType) {
        return getPluralAttribute("List", name, elementType, ListAttribute.class);
    }

    @Override
    public ListAttribute<? super X, ?> getList(String name) {
        return getList(name, Object.class);
    }

    @Override
    public <K, V> MapAttribute<? super X, K, V> getMap(String name, Class<K> keyType, Class<V> valueType) {
        throw new UnsupportedOperationException();
    }

    @Override
    public MapAttribute<? super X, ?, ?> getMap(String name) {
        return getMap(name, Object.class, Object.class);
    }

    @Override
    public <E> SetAttribute<? super X, E> getSet(String name, Class<E> elementType) {
        return getPluralAttribute("Set", name, elementType, SetAttribute.class);
    }

    @Override
    public SetAttribute<? super X, ?> getSet(String name) {
        return getSet(name, Object.class);
    }

    @Override
    public Set<PluralAttribute<? super X, ?, ?>> getPluralAttributes() {
        final Set<PluralAttribute<? super X, ?, ?>> plurals = new HashSet<>(getDeclaredPluralAttributes());
        if (classSupertype != null) {
            plurals.addAll(classSupertype.getPluralAttributes());
        }

        return plurals;
    }

    @Override
    public Set<SingularAttribute<? super X, ?>> getSingularAttributes() {
        final Set<SingularAttribute<? super X, ?>> singulars = new HashSet<>(getDeclaredSingularAttributes());
        if (classSupertype != null) {
            singulars.addAll(classSupertype.getSingularAttributes());
        }
        return singulars;
    }

    @Override
    public <Y> SingularAttribute<? super X, Y> getSingularAttribute(String name, Class<Y> type) {
        final Attribute<? super X, ?> a = getAttribute(name);

        if (a.isCollection()) {
            throw singularAttNotFound(name, type, false);
        }
        assert a instanceof SingularAttribute;
        if (!type.isAssignableFrom(a.getJavaType())) {
            throw singularAttNotFound(name, type, false);
        }

        return (SingularAttribute<? super X, Y>) a;
    }

    private IllegalArgumentException singularAttNotFound(String name, Class<?> type, boolean declared) {
        return new IllegalArgumentException("Singular attribute " + name + " of type " + type + " is not " + (declared ? "declared" : "present") + " in type " + this);
    }

    @Override
    public SingularAttribute<? super X, ?> getSingularAttribute(String name) {
        return getSingularAttribute(name, Object.class);
    }

    @Override
    public Set<Attribute<X, ?>> getDeclaredAttributes() {
        return getDeclaredAttributesImpl();
    }

    @Override
    public Set<PluralAttribute<X, ?, ?>> getDeclaredPluralAttributes() {
        return getDeclaredAttributesImpl().stream().filter(Attribute::isCollection)
                                          .map(a -> (PluralAttribute<X, ?, ?>) a).collect(Collectors.toSet());
    }

    @Override
    public Set<SingularAttribute<X, ?>> getDeclaredSingularAttributes() {
        return getDeclaredAttributesImpl().stream().filter(att -> !att.isCollection())
                                          .map(a -> (SingularAttribute<X, ?>) a).collect(Collectors.toSet());
    }

    @Override
    public AbstractAttribute<X, ?> getDeclaredAttribute(String name) {
        return (AbstractAttribute<X, ?>) getDeclaredAttributeImpl(name).orElseThrow(() -> attributeMissing(name, true));
    }

    @Override
    public <E> CollectionAttribute<X, E> getDeclaredCollection(String name, Class<E> elementType) {
        return getDeclaredPluralAttribute("Collection", name, elementType, CollectionAttribute.class);
    }

    private <E, R extends PluralAttribute<? super X, ?, E>> R getDeclaredPluralAttribute(String type, String name,
                                                                                         Class<E> elementType,
                                                                                         Class<R> attType) {
        final Attribute<? super X, ?> a = getDeclaredAttribute(name);

        checkPluralAttribute(a, type, name, elementType, attType, true);
        return attType.cast(a);
    }

    @Override
    public CollectionAttribute<X, ?> getDeclaredCollection(String name) {
        return getDeclaredCollection(name, Object.class);
    }

    @Override
    public <E> ListAttribute<X, E> getDeclaredList(String name, Class<E> elementType) {
        return getDeclaredPluralAttribute("List", name, elementType, ListAttribute.class);
    }

    @Override
    public ListAttribute<X, ?> getDeclaredList(String name) {
        return getDeclaredList(name, Object.class);
    }

    @Override
    public <K, V> MapAttribute<X, K, V> getDeclaredMap(String name, Class<K> keyType, Class<V> valueType) {
        throw new UnsupportedOperationException();
    }

    @Override
    public MapAttribute<X, ?, ?> getDeclaredMap(String name) {
        return getDeclaredMap(name, Object.class, Object.class);
    }

    @Override
    public <E> SetAttribute<X, E> getDeclaredSet(String name, Class<E> elementType) {
        return getDeclaredPluralAttribute("Set", name, elementType, SetAttribute.class);
    }

    @Override
    public SetAttribute<X, ?> getDeclaredSet(String name) {
        return getDeclaredSet(name, Object.class);
    }

    @Override
    public <Y> SingularAttribute<X, Y> getDeclaredSingularAttribute(String name, Class<Y> type) {
        final Attribute<X, ?> a = getDeclaredAttribute(name);

        if (a.isCollection()) {
            throw singularAttNotFound(name, type, true);
        }
        if (!type.isAssignableFrom(a.getJavaType())) {
            throw singularAttNotFound(name, type, true);
        }
        return (SingularAttribute<X, Y>) a;
    }

    @Override
    public SingularAttribute<X, ?> getDeclaredSingularAttribute(String name) {
        return getDeclaredSingularAttribute(name, Object.class);
    }

    @Override
    public TypesSpecification<? super X, ?> getTypes() {
        if (directTypes != null) {
            return directTypes;
        }
        return classSupertype != null ? classSupertype.getTypes() : null;
    }

    @Override
    public PropertiesSpecification<? super X, ?, ?, ?> getProperties() {
        if (properties != null) {
            return properties;
        }
        return classSupertype != null ? classSupertype.getProperties() : null;
    }

    @Override
    public Set<FieldSpecification<? super X, ?>> getFieldSpecifications() {
        final Set<FieldSpecification<? super X, ?>> specs = new HashSet<>(getAttributes());
        specs.addAll(getQueryAttributes());
        final TypesSpecification<? super X, ?> types = getTypes();
        if (types != null) {
            specs.add(types);
        }
        final PropertiesSpecification<? super X, ?, ?, ?> props = getProperties();
        if (props != null) {
            specs.add(props);
        }
        specs.add(getIdentifier());
        return specs;
    }

    @Override
    public FieldSpecification<? super X, ?> getFieldSpecification(String fieldName) {
        final Optional<AbstractAttribute<? super X, ?>> declaredAtt = getDeclaredAttributeImpl(fieldName);
        if (declaredAtt.isPresent()) {
            return declaredAtt.get();
        }
        if (declaredQueryAttributes.containsKey(fieldName)) {
            return declaredQueryAttributes.get(fieldName);
        }
        return getSpecialAttribute(fieldName).orElseGet(() -> {
            if (classSupertype != null) {
                return classSupertype.getFieldSpecification(fieldName, javaType);
            }

            throw new IllegalArgumentException("Field " + fieldName + " is not present in type " + this);
        });
    }

    private Optional<FieldSpecification<? super X, ?>> getSpecialAttribute(String fieldName) {
        if (declaredQueryAttributes.containsKey(fieldName)) {
            return Optional.of(declaredQueryAttributes.get(fieldName));
        }
        if (directTypes != null && directTypes.getName().equals(fieldName)) {
            return Optional.of(directTypes);
        } else if (properties != null && properties.getName().equals(fieldName)) {
            return Optional.of(properties);
        } else if (identifier != null && identifier.getName().equals(fieldName)) {
            return Optional.of(identifier);
        }
        return Optional.empty();
    }

    FieldSpecification<? super X, ?> getFieldSpecification(String fieldName, Class<? extends X> subtype) {
        final Optional<AbstractAttribute<? super X, ?>> att = getDeclaredAttribute(fieldName, subtype);
        if (att.isPresent()) {
            return att.get();
        }
        return getSpecialAttribute(fieldName).orElseGet(() -> {
            if (classSupertype != null) {
                return classSupertype.getFieldSpecification(fieldName, subtype);
            }

            throw new IllegalArgumentException("Field " + fieldName + " is not present in type " + this);
        });
    }

    @Override
    public Identifier<? super X, ?> getIdentifier() {
        if (identifier != null) {
            return identifier;
        }

        if (classSupertype != null) {
            return classSupertype.getIdentifier();
        }

        // This shouldn't happen, because every entity has to contain an identifier, otherwise metamodel building fails
        throw new IllegalArgumentException("Identifier attribute is not present in type " + this);
    }

    @Override
    public Class<X> getJavaType() {
        return javaType;
    }

    public EntityLifecycleListenerManager getLifecycleListenerManager() {
        return lifecycleListenerManager;
    }

    public void setLifecycleListenerManager(EntityLifecycleListenerManager lifecycleListenerManager) {
        this.lifecycleListenerManager = lifecycleListenerManager;
    }
}
