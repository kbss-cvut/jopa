package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.datatype.xsd.XsdDateTimeMapper;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.time.OffsetDateTime;
import java.util.Date;

/**
 * Converts between a xsd:dateTime representation and {@link Date} instances.
 * <p>
 * Supported representations are {@link OffsetDateTime} and {@link Literal}.
 * <p>
 * Note that when transforming to axiom value, UTC time zone is assumed to provide consistent results.
 */
public class DateConverter implements ConverterWrapper<Date, Object> {

    @Override
    public Literal convertToAxiomValue(Date value) {
        assert value != null;
        return InstantConverter.fromInstant(value.toInstant());
    }

    @Override
    public Date convertToAttribute(Object value) {
        assert value != null;
        if (value instanceof OffsetDateTime) {
            return offsetDateTimeToDate((OffsetDateTime) value);
        } else {
            assert value instanceof Literal;
            final Literal literal = (Literal) value;
            assert XSD.DATETIME.equals(literal.getDatatype());
            return offsetDateTimeToDate(XsdDateTimeMapper.map(literal.getLexicalForm()));
        }
    }

    private Date offsetDateTimeToDate(OffsetDateTime value) {
        return new Date(value.toInstant().toEpochMilli());
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return OffsetDateTime.class.isAssignableFrom(type) || Literal.class.isAssignableFrom(type);
    }
}
