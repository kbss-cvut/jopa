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
