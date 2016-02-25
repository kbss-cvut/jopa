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

import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.metamodel.ManagedType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Type;

import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.util.Collection;
import java.util.Map;

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

    private boolean includeExplicit;

    private boolean readOnly;

    private boolean nonEmpty;

    private ParticipationConstraint[] constraints;

    protected PluralAttributeImpl(PluralAttributeBuilder<X, C, E> builder) {
        this.elementType = builder.elementType;
        this.member = builder.field;
        assert member != null;
        this.name = member.getName();
        this.pat = builder.attributeType;
        this.collectionType = builder.collectionType;
        this.declaringType = builder.declaringType;
        this.iri = builder.iri;
        this.cascadeTypes = builder.cascadeTypes;
        this.fetchType = builder.fetchType;
        this.inferred = builder.inferred;
        this.includeExplicit = builder.includeExplicit;
        this.readOnly = builder.readOnly;
        this.nonEmpty = builder.nonEmpty;
        this.constraints = builder.constraints;
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


    public cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType getPersistentAttributeType() {
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


    public cz.cvut.kbss.jopa.model.metamodel.Bindable.BindableType getBindableType() {
        return BindableType.PLURAL_ATTRIBUTE;
    }


    public cz.cvut.kbss.jopa.model.metamodel.PluralAttribute.CollectionType getCollectionType() {
        if (getJavaType().isAssignableFrom(Collection.class)) {
            return CollectionType.COLLECTION;
        } else if (getJavaType().isAssignableFrom(Map.class)) {
            return CollectionType.MAP;
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

    @Override
    public boolean includeExplicit() {
        return includeExplicit;
    }

    @Override
    public boolean isReadOnly() {
        return readOnly;
    }

    @Override
    public boolean isNonEmpty() {
        return nonEmpty;
    }

    public ParticipationConstraint[] getConstraints() {
        return constraints;
    }

    public static PluralAttributeBuilder iri(IRI iri) {
        return new PluralAttributeBuilder().iri(iri);
    }


    public static class PluralAttributeBuilder<X, C, E> {
        private Type<E> elementType;
        private Class<C> collectionType;
        private Field field;
        private ManagedType<X> declaringType;
        private PersistentAttributeType attributeType;
        private IRI iri;
        private CascadeType[] cascadeTypes;
        private FetchType fetchType;
        private boolean inferred;
        private boolean includeExplicit;
        private boolean readOnly;
        private boolean nonEmpty;
        private ParticipationConstraint[] constraints;

        public PluralAttributeBuilder elementType(Type<E> elementType) {
            this.elementType = elementType;
            return this;
        }

        public PluralAttributeBuilder collectionType(Class<C> collectionType) {
            this.collectionType = collectionType;
            return this;
        }

        public PluralAttributeBuilder field(Field field) {
            this.field = field;
            return this;
        }

        public PluralAttributeBuilder declaringType(ManagedType<X> declaringType) {
            this.declaringType = declaringType;
            return this;
        }

        public PluralAttributeBuilder attributeType(PersistentAttributeType attributeType) {
            this.attributeType = attributeType;
            return this;
        }

        public PluralAttributeBuilder iri(IRI iri) {
            this.iri = iri;
            return this;
        }

        public PluralAttributeBuilder cascadeTypes(CascadeType[] cascadeTypes) {
            this.cascadeTypes = cascadeTypes;
            return this;
        }

        public PluralAttributeBuilder fetchType(FetchType fetchType) {
            this.fetchType = fetchType;
            return this;
        }

        public PluralAttributeBuilder inferred(boolean inferred) {
            this.inferred = inferred;
            return this;
        }

        public PluralAttributeBuilder includeExplicit(boolean includeExplicit) {
            this.includeExplicit = includeExplicit;
            return this;
        }

        public PluralAttributeBuilder readOnly(boolean readOnly) {
            this.readOnly = readOnly;
            return this;
        }

        public PluralAttributeBuilder nonEmpty(boolean nonEmpty) {
            this.nonEmpty = nonEmpty;
            return this;
        }

        public PluralAttributeBuilder participationConstraints(ParticipationConstraint[] constraints) {
            this.constraints = constraints;
            return this;
        }

        public PluralAttributeImpl<X, C, E> build() {
            return new PluralAttributeImpl<>(this);
        }
    }
}
