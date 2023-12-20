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
package cz.cvut.kbss.jopa.datatype.util;

import java.util.AbstractMap;

/**
 * Represents an ordered pair of values.
 *
 * @param <F> Type of the first value in the pair
 * @param <S> Type of the second value in the pair
 */
public final class Pair<F, S> extends AbstractMap.SimpleEntry<F, S> {

    public Pair(F first, S second) {
        super(first, second);
    }

    public F getFirst() {
        return getKey();
    }

    public S getSecond() {
        return getValue();
    }

    @Override
    public String toString() {
        return "(" + getFirst() + ", " + getSecond() + ")";
    }
}
