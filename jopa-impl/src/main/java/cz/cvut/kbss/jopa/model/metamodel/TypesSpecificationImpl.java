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

import cz.cvut.kbss.jopa.model.annotations.FetchType;

import java.lang.reflect.Field;
import java.util.Set;

public class TypesSpecificationImpl<X, Y> implements TypesSpecification<X, Y> {
    private final ManagedType<X> declaringType;
    private final FetchType fetchType;
    private final Field javaField;
    private Class<Y> elementType;
    private boolean inferred;

    public TypesSpecificationImpl(final ManagedType<X> declaringType,
                                  final FetchType fetchType, final Field javaField,
                                  final Class<Y> elementType, boolean inferred) {

        this.declaringType = declaringType;
        this.fetchType = fetchType;
        this.javaField = javaField;
        this.elementType = elementType;
        this.inferred = inferred;
    }

    @Override
    public ManagedType<X> getDeclaringType() {
        return declaringType;
    }

    @Override
    public FetchType getFetchType() {
        return fetchType;
    }

    @Override
    public Field getJavaField() {
        return javaField;
    }

    @Override
    public Class<Set> getJavaType() {
        return Set.class;
    }

    @Override
    public Class<Y> getElementType() {
        return elementType;
    }

    @Override
    public boolean isInferred() {
        return inferred;
    }

    @Override
    public boolean includeExplicit() {
        // TODO
        return true;
    }

    @Override
    public boolean isReadOnly() {
        // TODO
        return false;
    }

    @Override
    public String getName() {
        return javaField.getName();
    }

    @Override
    public boolean isCollection() {
        return true;
    }
}
