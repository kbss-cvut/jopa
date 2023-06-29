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
