/**
 * Copyright (C) 2011 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.owlapi;

import java.lang.reflect.Field;
import java.lang.reflect.Member;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.metamodel.ManagedType;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Type;

public class SingularAttributeImpl<X, T> implements SingularAttribute<X, T> {

    private final boolean id;

    private final String name;

    private final Type<T> type;

    private final Field field;

    private final ManagedType<X> declaringType;

    private final PersistentAttributeType attributeType;

    private final IRI iri;

    private final CascadeType[] cascadeTypes;

    private final FetchType fetchType;

    private final boolean inferred;

    private final boolean includeExplicit;

    private final boolean readOnly;

    private final boolean optional;

    private ParticipationConstraint[] constraints;

    private SingularAttributeImpl(SingularAttributeBuilder<X, T> builder) {
        this.id = builder.id;
        this.name = builder.name;
        this.type = builder.type;
        this.field = builder.field;
        this.declaringType = builder.declaringType;
        this.attributeType = builder.attributeType;
        this.iri = builder.iri;
        this.cascadeTypes = builder.cascadeTypes;
        this.fetchType = builder.fetchType;
        this.inferred = builder.inferred;
        this.includeExplicit = builder.includeExplicit;
        this.readOnly = builder.readOnly;
        this.constraints = builder.constraints;
        this.optional = builder.optional;
    }

    @Override
    public Type<T> getType() {
        return type;
    }

    @Override
    public boolean isId() {
        return id;
    }

    @Override
    public boolean isOptional() {
        return optional;
    }

    @Override
    public boolean isVersion() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ManagedType<X> getDeclaringType() {
        return declaringType;
    }

    @Override
    public Member getJavaMember() {
        return field;
    }

    @Override
    public Class<T> getJavaType() {
        return type.getJavaType();
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public PersistentAttributeType getPersistentAttributeType() {
        return attributeType;
    }

    @Override
    public boolean isAssociation() {
        return getPersistentAttributeType().equals(
                PersistentAttributeType.OBJECT);
    }

    @Override
    public boolean isCollection() {
        return false;
    }

    @Override
    public Class<T> getBindableJavaType() {
        return type.getJavaType();
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Bindable.BindableType getBindableType() {
        return BindableType.SINGULAR_ATTRIBUTE;
    }

    @Override
    public Field getJavaField() {
        return field;
    }

    @Override
    public IRI getIRI() {
        return iri;
    }

    @Override
    public CascadeType[] getCascadeTypes() {
        return cascadeTypes;
    }


    @Override
    public FetchType getFetchType() {
        return fetchType;
    }


    @Override
    public String toString() {
        return "SingularAttribute[" + name + "]";
    }

    @Override
    public boolean isInferred() {
        return inferred;
    }

    @Override
    public ParticipationConstraint[] getConstraints() {
        return constraints;
    }

    @Override
    public boolean includeExplicit() {
        return includeExplicit;
    }

    @Override
    public boolean isReadOnly() {
        return readOnly;
    }

    public static SingularAttributeBuilder iri(IRI iri) {
        return new SingularAttributeBuilder().iri(iri);
    }

    public static final class SingularAttributeBuilder<X, T> {
        private boolean id;
        private String name;
        private Type<T> type;
        private Field field;
        private ManagedType<X> declaringType;
        private PersistentAttributeType attributeType;
        private IRI iri;
        private CascadeType[] cascadeTypes;
        private FetchType fetchType;
        private boolean inferred;
        private boolean includeExplicit;
        private boolean readOnly;
        private boolean optional = true;
        private ParticipationConstraint[] constraints;

        public SingularAttributeBuilder identifier(boolean isId) {
            this.id = isId;
            return this;
        }

        public SingularAttributeBuilder name(String name) {
            this.name = name;
            return this;
        }

        public SingularAttributeBuilder type(Type<T> type) {
            this.type = type;
            return this;
        }

        public SingularAttributeBuilder field(Field field) {
            this.field = field;
            return this;
        }

        public SingularAttributeBuilder declaringType(ManagedType<X> declaringType) {
            this.declaringType = declaringType;
            return this;
        }

        public SingularAttributeBuilder attributeType(PersistentAttributeType attributeType) {
            this.attributeType = attributeType;
            return this;
        }

        public SingularAttributeBuilder iri(IRI iri) {
            this.iri = iri;
            return this;
        }

        public SingularAttributeBuilder<X, T> cascadeTypes(CascadeType[] cascadeTypes) {
            this.cascadeTypes = cascadeTypes;
            return this;
        }

        public SingularAttributeBuilder<X, T> fetchType(FetchType fetchType) {
            this.fetchType = fetchType;
            return this;
        }

        public SingularAttributeBuilder<X, T> inferred(boolean inferred) {
            this.inferred = inferred;
            return this;
        }

        public SingularAttributeBuilder<X, T> includeExplicit(boolean includeExplicit) {
            this.includeExplicit = includeExplicit;
            return this;
        }

        public SingularAttributeBuilder<X, T> readOnly(boolean readOnly) {
            this.readOnly = readOnly;
            return this;
        }

        public SingularAttributeBuilder<X, T> constraints(ParticipationConstraint[] constraints) {
            this.constraints = constraints;
            return this;
        }

        public SingularAttributeBuilder<X, T> option(boolean optional) {
            this.optional = optional;
            return this;
        }

        public SingularAttribute<X, T> build() {
            return new SingularAttributeImpl<>(this);
        }
    }
}
