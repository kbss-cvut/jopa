package cz.cvut.kbss.jopa.datatype;

import org.junit.jupiter.api.Test;

import java.time.*;

import static org.junit.jupiter.api.Assertions.assertEquals;

class DateTimeUtilTest {

    @Test
    void localDateTimeToDateTimeUsesSystemOffset() {
        final LocalDateTime value = LocalDateTime.now();
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(value);
        final OffsetDateTime expected = OffsetDateTime.of(value.toLocalDate(), value.toLocalTime(), offset);
        assertEquals(expected, DateTimeUtil.toDateTime(value));
    }

    @Test
    void instantToDateTimeAssumesUTCZone() {
        final Instant value = Instant.now();
        final OffsetDateTime expected = value.atOffset(ZoneOffset.UTC);
        assertEquals(expected, DateTimeUtil.toDateTime(value));
    }

    @Test
    void localTimeToTimeUsesSystemOffset() {
        final LocalTime value = LocalTime.now();
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now());
        final OffsetTime expected = OffsetTime.of(value, offset);
        assertEquals(expected, DateTimeUtil.toTime(value));
    }
}
