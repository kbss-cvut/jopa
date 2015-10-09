package cz.cvut.kbss.ontodriver.owlapi.util;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import java.net.URI;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

/**
 * Utility methods for the OWLAPI driver.
 */
public class OwlapiUtils {

    private static final String DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    /**
     * Creates OWLLiteral from the specified Java instance.
     *
     * @param value       The value to transform
     * @param dataFactory Data factory
     * @param lang        Ontology language
     * @return OWLLiteral representing the value
     * @throws IllegalArgumentException If {@code value} is of unsupported type
     */
    public static OWLLiteral createOWLLiteralFromValue(Object value, OWLDataFactory dataFactory, String lang) {
        Objects.requireNonNull(value);
        if (value instanceof Integer) {
            return dataFactory.getOWLLiteral((Integer) value);
        } else if (value instanceof Long) {
            return dataFactory.getOWLLiteral(value.toString(), OWL2Datatype.XSD_LONG);
        } else if (value instanceof Boolean) {
            return dataFactory.getOWLLiteral((Boolean) value);
        } else if (value instanceof Double) {
            return dataFactory.getOWLLiteral((Double) value);
        } else if (value instanceof String) {
            return dataFactory.getOWLLiteral((String) value, lang);
        } else if (value instanceof Date) {
            SimpleDateFormat sdf = new SimpleDateFormat(DATE_TIME_FORMAT);
            return dataFactory.getOWLLiteral(sdf.format(((Date) value)),
                    dataFactory.getOWLDatatype(OWL2Datatype.XSD_DATE_TIME.getIRI()));
        } else if (value.getClass().isEnum()) {
            return dataFactory.getOWLLiteral(value.toString());
        } else {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Transforms OWLLiteral to a plain Java object (boxed primitive or date/time).
     *
     * @param literal The literal to transform
     * @return Transformed value
     * @throws IllegalArgumentException If the literal is of unsupported type
     */
    public static Object owlLiteralToValue(final OWLLiteral literal) {
        if (literal.isRDFPlainLiteral()) {
            return literal.getLiteral();
        } else if (literal.getDatatype().isBuiltIn())
            switch (literal.getDatatype().getBuiltInDatatype()) {
                case XSD_SHORT:
                    return Short.parseShort(literal.getLiteral());
                case XSD_LONG:
                    return Long.parseLong(literal.getLiteral());
                case XSD_INT:
                case XSD_INTEGER:
                    return Integer.parseInt(literal.getLiteral());
                case XSD_DOUBLE:
                case XSD_DECIMAL:
                    return Double.parseDouble(literal.getLiteral());
                case XSD_FLOAT:
                    return Float.parseFloat(literal.getLiteral());
                case XSD_STRING:
                case RDF_XML_LITERAL:
                    return literal.getLiteral();
                case XSD_BOOLEAN:
                    return Boolean.parseBoolean(literal.getLiteral());
                case XSD_ANY_URI:
                    return URI.create(literal.getLiteral());
                case XSD_DATE_TIME_STAMP:
                case XSD_DATE_TIME:
                    try {
                        return new SimpleDateFormat(DATE_TIME_FORMAT).parse(literal.getLiteral());
                    } catch (ParseException e) {
                        throw new IllegalArgumentException(
                                "The date time '" + literal.getLiteral() + "' cannot be parsed.");
                    }
            }

        throw new IllegalArgumentException("Unsupported datatype: " + literal.getDatatype());
    }
}
