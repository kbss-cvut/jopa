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
package cz.cvut.kbss.jopa.datatype;

import java.time.*;
import java.util.Objects;

/**
 * Utility class for transformation between various date/time representations.
 */
public class DateTimeUtil {

    /**
     * System timezone offset used for transforming local datetime values to offset ones
     */
    public static final ZoneOffset SYSTEM_OFFSET = ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now());

    private DateTimeUtil() {
        throw new AssertionError();
    }

    /**
     * Transforms the specified {@link LocalDateTime} to an {@link OffsetDateTime}, using the system's default offset.
     *
     * @param value Datetime to transform
     * @return Offset datetime
     */
    public static OffsetDateTime toDateTime(LocalDateTime value) {
        Objects.requireNonNull(value);
        return OffsetDateTime.of(value.toLocalDate(), value.toLocalTime(), SYSTEM_OFFSET);
    }

    /**
     * Transforms the specified {@link Instant} to an {@link OffsetDateTime}.
     * <p>
     * The instant is taken to be at the UTC timezone.
     *
     * @param value Instant value to transform
     * @return Offset datetime
     */
    public static OffsetDateTime toDateTime(Instant value) {
        return Objects.requireNonNull(value).atOffset(ZoneOffset.UTC);
    }

    /**
     * Transforms the specified {@link LocalTime} to an {@link OffsetTime}, using the system's default offset.
     *
     * @param value Time to transform
     * @return Offset time
     */
    public static OffsetTime toTime(LocalTime value) {
        return OffsetTime.of(Objects.requireNonNull(value), SYSTEM_OFFSET);
    }
}
