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
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.query.sparql.SparqlConstants;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ParameterValueTest {

    @Test
    void toQueryValuesReturnsListOfSizeOneWithParameterValueStringifiedForQuery() {
        final ParameterValue sut = new IntegerParameterValue(Generators.randomPositiveInt(1000));
        final List<String> result = sut.toQueryValues(1);
        assertEquals(List.of(sut.getQueryString()), result);
    }

    @Test
    void toQueryValuesReturnsListOfSpecifiedSizeFilledWithValueAndUndef() {
        final ParameterValue sut = new IntegerParameterValue(Generators.randomPositiveInt(1000));
        int size = 5;
        final List<String> result = sut.toQueryValues(size);
        assertEquals(size, result.size());
        assertEquals(sut.getQueryString(), result.get(0));
        result.subList(1, size).forEach(v -> assertEquals(SparqlConstants.UNDEF, v));
    }
}
