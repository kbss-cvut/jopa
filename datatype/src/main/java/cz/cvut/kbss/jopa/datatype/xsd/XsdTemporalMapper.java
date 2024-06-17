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
package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.time.*;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalAmount;
import java.util.Objects;

import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.toDateTime;
import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.toTime;
import static java.time.format.DateTimeFormatter.*;

/**
 * Maps temporal values to their corresponding XSD datatype values.
 */
public final class XsdTemporalMapper {

    private XsdTemporalMapper() {
        throw new AssertionError();
    }

    /**
     * Maps the specified temporal value to a corresponding XSD literal.
     * <p>
     * Note that only a subset of {@link TemporalAccessor} subclasses is supported. This subset corresponds to the generic
     * date/time handling types declared in the {@code java.time} package (e.g., {@link LocalDateTime}, {@link OffsetDateTime} etc.).
     *
     * @param value Value to map
     * @return RDF literal with correct lexical form and datatype
     * @throws DatatypeMappingException When the temporal value is not supported
     */
    public static Literal map(TemporalAccessor value) {
        if (value instanceof OffsetDateTime) {
            return Literal.from(((OffsetDateTime) value).format(ISO_OFFSET_DATE_TIME), XSD.DATETIME);
        } else if (value instanceof LocalDateTime) {
            return Literal.from(toDateTime((LocalDateTime) value).format(ISO_OFFSET_DATE_TIME), XSD.DATETIME);
        } else if (value instanceof Instant) {
            return Literal.from(toDateTime((Instant) value).format(ISO_OFFSET_DATE_TIME), XSD.DATETIME);
        } else if (value instanceof ZonedDateTime) {
            return Literal.from(((ZonedDateTime) value).toOffsetDateTime().format(ISO_OFFSET_DATE_TIME), XSD.DATETIME);
        } else if (value instanceof OffsetTime) {
            return Literal.from(((OffsetTime) value).format(ISO_OFFSET_TIME), XSD.TIME);
        } else if (value instanceof LocalTime) {
            return Literal.from(toTime((LocalTime) value).format(ISO_OFFSET_TIME), XSD.TIME);
        } else if (value instanceof LocalDate) {
            return Literal.from(((LocalDate) value).format(ISO_DATE), XSD.DATE);
        }
        throw new DatatypeMappingException("Unsupported temporal accessor value " + value);
    }

    /**
     * Maps the specified temporal amount value to a corresponding XSD duration literal.
     *
     * @param value Value to map
     * @return RDF literal representing XSD duration value
     */
    public static Literal map(TemporalAmount value) {
        return Literal.from(Objects.requireNonNull(value).toString(), XSD.DURATION);
    }
}
