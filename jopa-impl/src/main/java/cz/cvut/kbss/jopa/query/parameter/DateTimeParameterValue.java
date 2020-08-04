/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.vocabulary.XSD;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.Objects;

/**
 * Parameter values which represent XSD dateTime.
 * <p>
 * Currently, these are {@link Date}, {@link LocalDateTime}, {@link java.time.OffsetDateTime} and {@link Instant}.
 */
class DateTimeParameterValue extends AbstractParameterValue {

    private final Object value;

    DateTimeParameterValue(Object value) {
        assert value instanceof Date || value instanceof LocalDateTime || value instanceof OffsetDateTime || value instanceof Instant;
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + (value instanceof Date ? ((Date) value).toInstant() : value) + "\"^^<" + XSD.DATETIME + ">";
    }
}
