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

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;

class CollectionParameterValueTest {

    private static final BooleanParameterValue V_ONE = new BooleanParameterValue(false);
    private static final IntegerParameterValue V_TWO = new IntegerParameterValue(117);
    private static final UriParameterValue V_THREE = new UriParameterValue(Generators.createIndividualIdentifier());

    @Test
    void getValueReturnsValuesOfIndividualElements() {
        final CollectionParameterValue sut = new CollectionParameterValue(List.of(V_ONE, V_TWO, V_THREE));

        assertEquals(List.of(V_ONE.getValue(), V_TWO.getValue(), V_THREE.getValue()), sut.getValue());
    }

    @Test
    void getQueryStringReturnsQueryStringRepresentationsOfIndividualElementsJoinedByComma() {
        final CollectionParameterValue sut = new CollectionParameterValue(List.of(V_ONE, V_TWO, V_THREE));

        assertEquals(
                Stream.of(V_ONE, V_TWO, V_THREE).map(ParameterValue::getQueryString).collect(Collectors.joining(",")),
                sut.getQueryString());
    }

    @Test
    void toQueryValuesReturnsListOfQueryStringifiedParameterValues() {
        final CollectionParameterValue sut = new CollectionParameterValue(List.of(V_ONE, V_TWO, V_THREE));
        final List<String> result = sut.toQueryValues(sut.valueCount());
        assertEquals(Stream.of(V_ONE, V_TWO, V_THREE).map(ParameterValue::getQueryString).toList(), result);
    }

    @Test
    void toQueryValuesReturnsListOfQueryStringifiedParameterValuesFilledWithSparqlUndefWhenSizeIsGreaterThanValueCount() {
        final int size = 6;
        final CollectionParameterValue sut = new CollectionParameterValue(List.of(V_ONE, V_TWO, V_THREE));
        final List<String> result = sut.toQueryValues(size);
        assertThat(result, hasItems(Stream.of(V_ONE, V_TWO, V_THREE).map(ParameterValue::getQueryString).toArray(String[]::new)));
        final String[] remainder = new String[size - sut.valueCount()];
        Arrays.fill(remainder, SparqlConstants.UNDEF);
        assertThat(result, hasItems(remainder));
    }
}
