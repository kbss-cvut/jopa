/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.datatype.xsd.XsdTemporalMapper;

import java.time.temporal.TemporalAmount;
import java.util.Objects;

/**
 * Duration query parameter value representation.
 * <p>
 * Works for both {@link java.time.Duration} and {@link java.time.Period}, which are mapped to XSD duration.
 */
public class DurationParameterValue extends AbstractParameterValue {

    private final TemporalAmount value;

    public DurationParameterValue(TemporalAmount value) {
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
