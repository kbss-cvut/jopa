/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.environment;

import java.util.Map;
import java.util.Set;

public class TestEnvironmentUtils {

    private TestEnvironmentUtils() {
        throw new AssertionError();
    }

    public static <K, V> boolean arePropertiesEqual(Map<K, Set<V>> pOne, Map<K, Set<V>> pTwo) {
        if (pOne.size() != pTwo.size()) {
            return false;
        }
        for (Map.Entry<K, Set<V>> e : pOne.entrySet()) {
            if (!pTwo.containsKey(e.getKey())) {
                return false;
            }
            final Set<?> set = pTwo.get(e.getKey());
            if (!e.getValue().equals(set)) {
                return false;
            }
        }
        return true;
    }

    public static <E> boolean typesEqual(Set<E> expected, Set<E> actual) {
        if (expected == null && actual != null || expected != null && actual == null) {
            return false;
        }
        return expected == null || expected.size() == actual.size() && expected.containsAll(actual);
    }
}
