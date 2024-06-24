/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment.utils;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

import java.util.Collection;
import java.util.Objects;

/**
 * Checks whether the provided collection contains the same entities as the expected one.
 * <p>
 * The membership check is done based on entity URIs.
 * <p>
 * Item order is not significant in the comparison, but the total number of items is.
 */
public class ContainsSameEntities extends TypeSafeMatcher<Collection<? extends HasUri>> {

    private final Collection<? extends HasUri> expected;

    public ContainsSameEntities(Collection<? extends HasUri> expected) {
        this.expected = Objects.requireNonNull(expected);
    }

    @Override
    protected boolean matchesSafely(Collection<? extends HasUri> actual) {
        if (actual == null || actual.size() != expected.size()) {
            return false;
        }
        for (HasUri e : expected) {
            if (actual.stream().noneMatch(ee -> Objects.equals(e.getUri(), ee.getUri()))) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void describeTo(Description description) {
        description.appendValueList("[", ", ", "]", expected);
    }

    public static ContainsSameEntities containsSameEntities(Collection<? extends HasUri> expected) {
        return new ContainsSameEntities(expected);
    }
}
