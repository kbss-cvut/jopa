package cz.cvut.kbss.ontodriver.owlapi.util;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Utility methods for the OWLAPI driver.
 */
public class OwlapiUtils {

    /**
     * Creates OWLLiteral from the specified Java instance.
     *
     * @param value
     *            The value to transform
     * @param dataFactory
     *            Data factory
     * @param lang
     *            Ontology language
     * @return OWLLiteral representing the value
     * @throws IllegalArgumentException
     *             If {@code value} is of unsupported type
     */
    public static OWLLiteral createOWLLiteralFromValue(Object value, OWLDataFactory dataFactory,
                                                       String lang) {
        if (value instanceof Integer) {
            return dataFactory.getOWLLiteral((Integer) value);
        } else if (value instanceof Boolean) {
            return dataFactory.getOWLLiteral((Boolean) value);
        } else if (value instanceof Double) {
            return dataFactory.getOWLLiteral((Double) value);
        } else if (value instanceof String) {
            return dataFactory.getOWLLiteral((String) value, lang);
        } else if (value instanceof Date) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");
            return dataFactory.getOWLLiteral(sdf.format(((Date) value)),
                    dataFactory.getOWLDatatype(OWL2Datatype.XSD_DATE_TIME.getIRI()));
        } else {
            throw new IllegalArgumentException();
        }
    }
}
