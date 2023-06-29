/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

public class SingularAttributeImpl<X, Y> extends AbstractAttribute<X, Y> implements SingularAttribute<X, Y> {

    private final Type<Y> type;

    private SingularAttributeImpl(SingularAttributeBuilder<X, Y> builder) {
        super(builder);
        this.type = builder.type;
    }

    @Override
    public Type<Y> getType() {
        return type;
    }

    @Override
    public boolean isId() {
        return false;
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
    public boolean isCollection() {
        return false;
    }

    @Override
    public Class<Y> getBindableJavaType() {
        return type.getJavaType();
    }

    @Override
    public BindableType getBindableType() {
        return BindableType.SINGULAR_ATTRIBUTE;
    }

    public static SingularAttributeBuilder builder(PropertyAttributes config) {
        return new SingularAttributeBuilder().config(config);
    }

    public static final class SingularAttributeBuilder<X, Y> extends AbstractAttributeBuilder<X, Y> {
        private Type<Y> type;

        @Override
        public SingularAttributeBuilder<X, Y> config(PropertyAttributes config) {
            super.config(config);
            type((Type<Y>) config.getType());
            return this;
        }

        public SingularAttributeBuilder<X, Y> type(Type<Y> type) {
            this.type = type;
            return this;
        }

        @Override
        public SingularAttributeBuilder<X, Y> propertyInfo(PropertyInfo propertyInfo) {
            super.propertyInfo(propertyInfo);
            return this;
        }

        @Override
        public SingularAttributeBuilder<X, Y> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        @Override
        public SingularAttributeBuilder<X, Y> inferred(boolean inferred) {
            super.inferred(inferred);
            return this;
        }

        @Override
        public SingularAttributeBuilder<X, Y> includeExplicit(boolean includeExplicit) {
            super.includeExplicit(includeExplicit);
            return this;
        }

        @Override
        public SingularAttributeBuilder<X, Y> converter(ConverterWrapper converter) {
            super.converter(converter);
            return this;
        }

        @Override
        public SingularAttributeImpl<X, Y> build() {
            final SingularAttributeImpl<X, Y> result = new SingularAttributeImpl<>(this);
            mappingValidator.validateAttributeMapping(result);
            return result;
        }
    }
}
