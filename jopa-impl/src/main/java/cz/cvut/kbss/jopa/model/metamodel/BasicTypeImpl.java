/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

public class BasicTypeImpl<X> implements BasicType<X> {

    private final Class<X> c;

    BasicTypeImpl(Class<X> c) {
        this.c = c;
    }

    @Override
    public Class<X> getJavaType() {
        return c;
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Type.PersistenceType getPersistenceType() {
        return PersistenceType.BASIC;
    }

    public static <X> BasicType<X> get(final Class<X> c) {
        return new BasicTypeImpl<>(c);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((c == null) ? 0 : c.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        BasicTypeImpl<?> other = (BasicTypeImpl<?>) obj;
        if (c == null) {
            return other.c == null;
        } else return c.equals(other.c);
    }
}
