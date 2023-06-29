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

import cz.cvut.kbss.jopa.model.annotations.FetchType;

import java.lang.reflect.Field;

public class IRIIdentifierImpl<T> implements IRIIdentifier {

    private final ManagedType<T> declaringType;
    private final Field javaField;

    private final boolean generated;

    public IRIIdentifierImpl(ManagedType<T> declaringType, final Field javaField, final boolean generated) {
        this.declaringType = declaringType;
        this.javaField = javaField;
        this.generated = generated;
    }

    @Override
    public ManagedType<T> getDeclaringType() {
        return declaringType;
    }

    @Override
    public Class<?> getJavaType() {
        return javaField.getType();
    }

    @Override
    public Field getJavaField() {
        return javaField;
    }

    @Override
    public FetchType getFetchType() {
        return FetchType.EAGER;
    }

    @Override
    public boolean isInferred() {
        return false;
    }

    @Override
    public boolean includeExplicit() {
        return true;
    }

    @Override
    public String getName() {
        return javaField.getName();
    }

    @Override
    public boolean isCollection() {
        return false;
    }

    @Override
    public void accept(IdentifierVisitor i) {
        i.visit(this);
    }

    @Override
    public boolean isGenerated() {
        return generated;
    }

    @Override
    public BindableType getBindableType() {
        return BindableType.SINGULAR_ATTRIBUTE;
    }

    @Override
    public Class<?> getBindableJavaType() {
        return getJavaType();
    }
}
