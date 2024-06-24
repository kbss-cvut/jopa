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

import cz.cvut.kbss.jopa.datatype.xsd.XsdTemporalMapper;

import java.time.temporal.TemporalAccessor;
import java.util.Objects;

/**
 * Query parameter value representation for date/time.
 * <p>
 * Based on the Java 8 {@link TemporalAccessor} implementations. Query string mapping uses XSD datatypes.
 */
public class TemporalParameterValue extends AbstractParameterValue {

    private final TemporalAccessor value;

    public TemporalParameterValue(TemporalAccessor value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return XsdTemporalMapper.map(value).toString();
    }
}
