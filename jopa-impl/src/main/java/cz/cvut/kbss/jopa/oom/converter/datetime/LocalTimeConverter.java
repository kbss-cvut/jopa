package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.datatype.xsd.XsdTimeMapper;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.time.LocalTime;
import java.time.OffsetTime;

/**
 * Converts between Java 8 {@link LocalTime} and a supported xsd:time representation.
 * <p>
 * This converter supports {@link OffsetTime} and {@link cz.cvut.kbss.ontodriver.model.Literal} as date time.
 */
public class LocalTimeConverter implements ConverterWrapper<LocalTime, Object> {

    @Override
    public Object convertToAxiomValue(LocalTime value) {
        // Let the OntoDriver take care of conversion to a correct repository value
        return value;
    }

    @Override
    public LocalTime convertToAttribute(Object value) {
        assert value != null;
        if (value instanceof OffsetTime) {
            return ((OffsetTime) value).toLocalTime();
        } else {
            assert value instanceof Literal;
            final Literal literal = (Literal) value;
            assert XSD.TIME.equals(literal.getDatatype());
            return (XsdTimeMapper.map(literal.getLexicalForm())).toLocalTime();
        }
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return OffsetTime.class.isAssignableFrom(type) || Literal.class.isAssignableFrom(type);
    }
}
