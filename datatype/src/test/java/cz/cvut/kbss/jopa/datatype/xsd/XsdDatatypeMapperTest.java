package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;
import java.time.LocalDate;
import java.time.OffsetTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class XsdDatatypeMapperTest {

    private final XsdDatatypeMapper sut = XsdDatatypeMapper.getInstance();

    @Test
    void mapReturnsUnsignedByteAsShort() {
        final byte value = 115;
        final Literal literal = Literal.from(Byte.toString(value), XSD.UNSIGNED_BYTE);
        assertEquals((short) value, map(literal));
    }

    private Object map(Literal literal) {
        final Optional<Object> result = sut.map(literal);
        assertTrue(result.isPresent());
        return result.get();
    }

    @Test
    void mapReturnsEmptyOptionalForUnknownDatatype() {
        final String datatype = XSD.NAMESPACE + "NOTATION";
        assertFalse(sut.map(Literal.from("jpeg", datatype)).isPresent());
    }

    @Test
    void mapReturnsUnsignedShortAsInt() {
        final short value = 1234;
        final Literal literal = Literal.from(Short.toString(value), XSD.UNSIGNED_SHORT);
        assertEquals((int) value, map(literal));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionForInvalidLiteralValue() {
        final Literal invalidValue = Literal.from("abcd", XSD.INT);
        assertThrows(DatatypeMappingException.class, () -> sut.map(invalidValue));
    }

    @Test
    void mapReturnsUnsignedIntAsLong() {
        final int value = Integer.MAX_VALUE;
        final Literal literal = Literal.from(Integer.toString(value), XSD.UNSIGNED_INT);
        assertEquals((long) value, map(literal));
    }

    @Test
    void mapReturnsIntegerAsBigInteger() {
        final int value = Integer.MIN_VALUE;
        final Literal literal = Literal.from(Integer.toString(value), XSD.INTEGER);
        assertEquals(BigInteger.valueOf(value), map(literal));
    }

    @Test
    void mapReturnsUnsignedLongAsBigInteger() {
        final long value = System.currentTimeMillis();
        final Literal literal = Literal.from(Long.toString(value), XSD.UNSIGNED_LONG);
        assertEquals(BigInteger.valueOf(value), map(literal));
    }

    @Test
    void mapReturnsXsdStringAsString() {
        final String value = "test";
        final Literal literal = Literal.from(value, XSD.STRING);
        assertEquals(value, map(literal));
    }

    @Test
    void mapReturnsXsdNormalizedStringAsString() {
        final String value = "test";
        final Literal literal = Literal.from(value, XSD.NORMALIZED_STRING);
        assertEquals(value, map(literal));
    }

    @Test
    void mapReturnsLocalDateForXsdDate() {
        final LocalDate date = LocalDate.now();
        final Literal literal = Literal.from(date.format(DateTimeFormatter.ISO_DATE), XSD.DATE);
        assertEquals(date, map(literal));
    }

    @Test
    void mapReturnsOffsetTimeForXsdTime() {
        final OffsetTime time = OffsetTime.of(12, 45, 15, 0, ZoneOffset.UTC);
        assertEquals(time, map(Literal.from(time.format(DateTimeFormatter.ISO_TIME), XSD.TIME)));
    }
}
