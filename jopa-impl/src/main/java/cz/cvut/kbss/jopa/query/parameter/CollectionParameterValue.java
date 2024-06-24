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
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.query.sparql.SparqlConstants;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class CollectionParameterValue extends AbstractParameterValue {

    private final List<ParameterValue> values;

    CollectionParameterValue(List<ParameterValue> values) {
        this.values = values;
    }

    @Override
    public List<Object> getValue() {
        return values.stream().map(ParameterValue::getValue).collect(Collectors.toList());
    }

    @Override
    public String getQueryString() {
        return values.stream().map(ParameterValue::getQueryString).collect(Collectors.joining(","));
    }

    @Override
    public List<String> toQueryValues(int size) {
        assert size >= values.size();
        final List<String> result = new ArrayList<>(size);
        values.stream().map(ParameterValue::getQueryString).forEach(result::add);
        if (values.size() < size) {
            for (int i = values.size(); i < size; i++) {
                result.add(SparqlConstants.UNDEF);
            }
        }
        return result;
    }

    @Override
    public int valueCount() {
        return values.size();
    }
}
