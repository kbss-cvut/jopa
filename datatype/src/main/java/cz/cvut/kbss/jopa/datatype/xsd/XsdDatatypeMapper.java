package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.DatatypeMapper;
import cz.cvut.kbss.jopa.datatype.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;
import java.util.Optional;

/**
 * Maps XML Schema types to Java.
 * <p>
 * The mapping logic is based on the known <a href="https://docs.oracle.com/javase/tutorial/jaxb/intro/bind.html">JAXB</a>/
 * <a href="https://xmlbeans.apache.org/docs/2.0.0/guide/conXMLBeansSupportBuiltInSchemaTypes.html">Apache XML Beans</a>
 * mapping with the following exceptions:
 * <ul>
 *     <li>Java 8 date/time API is utilized</li>
 *     <li>Mapping used by libraries such as Jena/RDF4J/OWL API is taken into account</li>
 * </ul>
 */
public class XsdDatatypeMapper implements DatatypeMapper {

    private static final XsdDatatypeMapper INSTANCE = new XsdDatatypeMapper();

    /**
     * Gets an instance of this mapper.
     * <p>
     * Convenience method returning one shared instance (the mapper has no state and is thread-safe).
     *
     * @return Shared mapper instance
     */
    public static XsdDatatypeMapper getInstance() {
        return INSTANCE;
    }

    @Override
    public Optional<Object> map(Literal literal) {
        Objects.requireNonNull(literal);
        final String value = literal.getLexicalForm();
        try {
            switch (literal.getDatatype()) {
                case XSD.BOOLEAN:
                    return Optional.of(Boolean.parseBoolean(value));
                case XSD.BYTE:
                    return Optional.of(Byte.parseByte(value));
                case XSD.SHORT:
                case XSD.UNSIGNED_BYTE:
                    return Optional.of(Short.parseShort(value));
                case XSD.INT:
                case XSD.UNSIGNED_SHORT:
                    return Optional.of(Integer.parseInt(value));
                case XSD.LONG:
                case XSD.UNSIGNED_INT:
                    return Optional.of(Long.parseLong(value));
                case XSD.FLOAT:
                    return Optional.of(Float.parseFloat(value));
                case XSD.DOUBLE:
                    return Optional.of(Double.parseDouble(value));
                case XSD.STRING:
                case XSD.NORMALIZED_STRING:
                    return Optional.of(value);
                case XSD.DATE:
                    return Optional.of(XsdDateMapper.map(value));
                case XSD.TIME:
                    return Optional.of(XsdTimeMapper.map(value));
                case XSD.INTEGER:
                case XSD.NON_NEGATIVE_INTEGER:
                case XSD.NON_POSITIVE_INTEGER:
                case XSD.NEGATIVE_INTEGER:
                case XSD.POSITIVE_INTEGER:
                case XSD.UNSIGNED_LONG:
                    return Optional.of(new BigInteger(value));
                case XSD.DECIMAL:
                    return Optional.of(new BigDecimal(value));
                default:
                    return Optional.empty();
            }
        } catch (NumberFormatException e) {
            throw new DatatypeMappingException("Unable to map literal " + literal, e);
        }
    }
}
