/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import java.util.*;
import java.util.stream.Collectors;

/**
 * Instances of the type AbstractIdentifiableType represent entity or mapped superclass
 * types which can be queried for attributes, subtypes and so on.
 *
 * @param <X> Entity type being represented by this instance
 */

public abstract class AbstractIdentifiableType<X> implements IdentifiableType<X> {

    private final Class<X> javaType;

    private Identifier<X, ?> identifier;

    private Set<AbstractIdentifiableType<? super X>> supertypes = new HashSet<>();

    /// AIT which is superclass of this AIT, and from which we inherit attributes
    private AbstractIdentifiableType<? super X> classSupertype;

    private Set<AbstractIdentifiableType<? extends X>> subtypes;

    private TypesSpecification<X, ?> directTypes;

    private PropertiesSpecification<X, ?, ?, ?> properties;

    private final Map<String, AbstractAttribute<X, ?>> declaredAttributes = new HashMap<>();

    private final Map<String, AbstractQueryAttribute<X, ?>> declaredQueryAttributes = new HashMap<>();

    private EntityLifecycleListenerManager lifecycleListenerManager = EntityLifecycleListenerManager.empty();

    AbstractIdentifiableType(Class<X> javaType) {
        this.javaType = javaType;
    }

    void addDeclaredAttribute(final String name, final AbstractAttribute<X, ?> a) {
        declaredAttributes.put(name, a);
    }

    void addDeclaredQueryAttribute(final String name, final AbstractQueryAttribute<X, ?> a) {
        declaredQueryAttributes.put(name, a);
    }

    /**
     * Set supertypes of this instance, and for all given supertypes add this as their subtype.
     * This method should not be called multiple times on one instance.
     *
     * @param supertypes Supertypes to set
     */
    void setSupertypes(Set<AbstractIdentifiableType<? super X>> supertypes) {
        assert supertypes != null;
        this.supertypes = supertypes;

        supertypes.forEach(supertype -> supertype.addSubtype(this));
        /// find non-abstract parent (class), and use it later for finding attributes, as attributes can be only in AITs that represent classes
        supertypes.stream().filter(ait -> !ait.getJavaType().isInterface()).findAny().ifPresent(clsSupertype -> this.classSupertype = clsSupertype);
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

    public void setIdentifier(final Identifier<X, ?> identifier) {
        this.identifier = identifier;
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
        return subtypes != null;
    }

    /**
     * Whether the Java type represented by this type is an abstract class.
     *
     * @return {@code true} if the represented Java type is abstract, {@code false} otherwise
     */
    public abstract boolean isAbstract();

    public Set<AbstractIdentifiableType<? extends X>> getSubtypes() {
        return subtypes != null ? Collections.unmodifiableSet(subtypes) : Collections.emptySet();
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
        final Set<Attribute<? super X, ?>> attributes = new HashSet<>(declaredAttributes.values());
        if (classSupertype != null) {
            attributes.addAll(classSupertype.getAttributes());
        }

        return attributes;
    }

    @Override
    public Set<QueryAttribute<? super X, ?>> getQueryAttributes() {
        final Set<QueryAttribute<? super X, ?>> queryAttributes = new HashSet<>(declaredQueryAttributes.values());

        if (classSupertype != null) {
            queryAttributes.addAll(classSupertype.getQueryAttributes());
        }
        return queryAttributes;
    }

    @Override
    public AbstractAttribute<? super X, ?> getAttribute(String name) {
        Objects.requireNonNull(name);
        if (declaredAttributes.containsKey(name)) {
            return declaredAttributes.get(name);
        }

        if (classSupertype != null) {
            return classSupertype.getAttribute(name);
        }
        throw attributeMissing(name, false);
    }

    protected IllegalArgumentException attributeMissing(String name, boolean declared) {
        return new IllegalArgumentException("Attribute " + name + " is not " + (declared ? "declared" : "present") + " in type " + this);
    }

    @Override
    public boolean hasQueryAttribute(String name) {
        Objects.requireNonNull(name);
        if (declaredQueryAttributes.containsKey(name)) {
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
        if (classSupertype != null) {
            return classSupertype.getQueryAttribute(name);
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

    private <E, R extends PluralAttribute<? super X, ?, E>> R getPluralAttribute(String type, String name, Class<E> elementType, Class<R> attType) {
        final Attribute<? super X, ?> a = getAttribute(name);

        checkPluralAttribute(a, type, name, elementType, attType, false);
        return attType.cast(a);
    }

    private <E, R extends PluralAttribute<? super X, ?, E>> void checkPluralAttribute(Attribute<? super X, ?> att, String type, String name, Class<E> elementType, Class<R> attType, boolean declared) {
        if (!attType.isAssignableFrom(att.getClass())) {
            throw pluralAttNotFound(type, name, elementType, declared);
        }

        final PluralAttribute<? super X, ?, E> colAtt = (PluralAttribute<? super X, ?, E>) att;
        if (!elementType.isAssignableFrom(colAtt.getBindableJavaType())) {
            throw pluralAttNotFound(type, name, elementType, declared);
        }
    }

    private IllegalArgumentException pluralAttNotFound(String type, String name, Class<?> elementType, boolean declared) {
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
        return new HashSet<>(declaredAttributes.values());
    }

    @Override
    public Set<PluralAttribute<X, ?, ?>> getDeclaredPluralAttributes() {
        return declaredAttributes.values().stream().filter(Attribute::isCollection).map(a -> (PluralAttribute<X, ?, ?>) a).collect(Collectors.toSet());
    }

    @Override
    public Set<SingularAttribute<X, ?>> getDeclaredSingularAttributes() {
        return declaredAttributes.values().stream().filter(att -> !att.isCollection()).map(a -> (SingularAttribute<X, ?>) a).collect(Collectors.toSet());
    }

    @Override
    public AbstractAttribute<X, ?> getDeclaredAttribute(String name) {
        if (declaredAttributes.containsKey(name)) {
            return declaredAttributes.get(name);
        }
        throw attributeMissing(name, true);
    }

    @Override
    public <E> CollectionAttribute<X, E> getDeclaredCollection(String name, Class<E> elementType) {
        return getDeclaredPluralAttribute("Collection", name, elementType, CollectionAttribute.class);
    }

    private <E, R extends PluralAttribute<? super X, ?, E>> R getDeclaredPluralAttribute(String type, String name, Class<E> elementType, Class<R> attType) {
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
        if (declaredAttributes.containsKey(fieldName)) {
            return declaredAttributes.get(fieldName);
        }
        if (declaredQueryAttributes.containsKey(fieldName)) {
            return declaredQueryAttributes.get(fieldName);
        }
        if (directTypes != null && directTypes.getName().equals(fieldName)) {
            return directTypes;
        } else if (properties != null && properties.getName().equals(fieldName)) {
            return properties;
        } else if (identifier != null && identifier.getName().equals(fieldName)) {
            return identifier;
        }
        if (classSupertype != null) {
            return classSupertype.getFieldSpecification(fieldName);
        }

        throw new IllegalArgumentException("Field " + fieldName + " is not present in type " + this);
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
