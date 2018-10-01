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

import cz.cvut.kbss.jopa.model.AttributeConverter;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;

import java.lang.reflect.Field;

public class SingularAttributeImpl<X, Y> extends AbstractAttribute<X, Y> implements SingularAttribute<X, Y> {

    private final boolean id;

    private final Type<Y> type;

    private SingularAttributeImpl(SingularAttributeBuilder<X, Y> builder) {
        super(builder);
        this.id = builder.id;
        this.type = builder.type;
    }

    @Override
    public Type<Y> getType() {
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
    public Class<Y> getJavaType() {
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
    public Class<Y> getBindableJavaType() {
        return type.getJavaType();
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Bindable.BindableType getBindableType() {
        return BindableType.SINGULAR_ATTRIBUTE;
    }

    public static SingularAttributeBuilder iri(IRI iri) {
        return new SingularAttributeBuilder().iri(iri);
    }

    public static final class SingularAttributeBuilder<X, Y> extends AbstractAttributeBuilder<X, Y> {
        private boolean id;
        private Type<Y> type;

        public SingularAttributeBuilder<X, Y> identifier(boolean isId) {
            this.id = isId;
            return this;
        }

        @Override
        public SingularAttributeBuilder<X, Y> name(String name) {
            super.name(name);
            return this;
        }

        public SingularAttributeBuilder<X, Y> type(Type<Y> type) {
            this.type = type;
            return this;
        }

        public SingularAttributeBuilder<X, Y> field(Field field) {
            super.field(field);
            return this;
        }

        public SingularAttributeBuilder<X, Y> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        public SingularAttributeBuilder<X, Y> attributeType(PersistentAttributeType attributeType) {
            super.attributeType(attributeType);
            return this;
        }

        public SingularAttributeBuilder<X, Y> iri(IRI iri) {
            super.iri(iri);
            return this;
        }

        public SingularAttributeBuilder<X, Y> cascadeTypes(CascadeType[] cascadeTypes) {
            super.cascadeTypes(cascadeTypes);
            return this;
        }

        public SingularAttributeBuilder<X, Y> fetchType(FetchType fetchType) {
            super.fetchType(fetchType);
            return this;
        }

        public SingularAttributeBuilder<X, Y> inferred(boolean inferred) {
            super.inferred(inferred);
            return this;
        }

        public SingularAttributeBuilder<X, Y> includeExplicit(boolean includeExplicit) {
            super.includeExplicit(includeExplicit);
            return this;
        }

        public SingularAttributeBuilder<X, Y> constraints(ParticipationConstraint[] constraints) {
            super.constraints(constraints);
            return this;
        }

        public SingularAttributeBuilder<X, Y> nonEmpty(boolean nonEmpty) {
            super.nonEmpty(nonEmpty);
            return this;
        }

        @Override
        public SingularAttributeBuilder<X, Y> converter(AttributeConverter<Y, ?> converter) {
            super.converter(converter);
            return this;
        }

        public SingularAttribute<X, Y> build() {
            return new SingularAttributeImpl<>(this);
        }
    }
}
