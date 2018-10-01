/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;

import java.lang.reflect.Field;

public class SingularAttributeImpl<X, T> extends AbstractAttribute<X, T> implements SingularAttribute<X, T> {

    private final boolean id;

    private final Type<T> type;

    private SingularAttributeImpl(SingularAttributeBuilder<X, T> builder) {
        super(builder);
        this.id = builder.id;
        this.type = builder.type;
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
    public boolean isVersion() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Class<T> getJavaType() {
        return type.getJavaType();
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

    public static SingularAttributeBuilder iri(IRI iri) {
        return new SingularAttributeBuilder().iri(iri);
    }

    public static final class SingularAttributeBuilder<X, T> extends AbstractAttributeBuilder<X, T> {
        private boolean id;
        private Type<T> type;

        public SingularAttributeBuilder<X, T> identifier(boolean isId) {
            this.id = isId;
            return this;
        }

        public SingularAttributeBuilder<X, T> name(String name) {
            super.name(name);
            return this;
        }

        public SingularAttributeBuilder<X, T> type(Type<T> type) {
            this.type = type;
            return this;
        }

        public SingularAttributeBuilder<X, T> field(Field field) {
            super.field(field);
            return this;
        }

        public SingularAttributeBuilder<X, T> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        public SingularAttributeBuilder<X, T> attributeType(PersistentAttributeType attributeType) {
            super.attributeType(attributeType);
            return this;
        }

        public SingularAttributeBuilder<X, T> iri(IRI iri) {
            super.iri(iri);
            return this;
        }

        public SingularAttributeBuilder<X, T> cascadeTypes(CascadeType[] cascadeTypes) {
            super.cascadeTypes(cascadeTypes);
            return this;
        }

        public SingularAttributeBuilder<X, T> fetchType(FetchType fetchType) {
            super.fetchType(fetchType);
            return this;
        }

        public SingularAttributeBuilder<X, T> inferred(boolean inferred) {
            super.inferred(inferred);
            return this;
        }

        public SingularAttributeBuilder<X, T> includeExplicit(boolean includeExplicit) {
            super.includeExplicit(includeExplicit);
            return this;
        }

        public SingularAttributeBuilder<X, T> constraints(ParticipationConstraint[] constraints) {
            super.constraints(constraints);
            return this;
        }

        public SingularAttributeBuilder<X, T> nonEmpty(boolean nonEmpty) {
            super.nonEmpty(nonEmpty);
            return this;
        }

        public SingularAttribute<X, T> build() {
            return new SingularAttributeImpl<>(this);
        }
    }
}
