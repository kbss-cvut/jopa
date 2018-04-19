package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;

import java.net.URL;

/**
 * Utility methods for working with Jena API.
 */
public class JenaUtils {

    private JenaUtils() {
        throw new AssertionError();
    }

    /**
     * Resolves whether the specified value is a resource identifier.
     * <p>
     * Only absolute IRIs are supported (i.e. no blank node identifiers).
     *
     * @param value The value to check
     * @return {@code true} if the value is either an URI or an URL
     */
    public static boolean isResourceIdentifier(Object value) {
        if (value instanceof NamedResource || value instanceof java.net.URI || value instanceof URL) {
            return true;
        }
        if (!(value instanceof String)) {
            return false;
        }
        try {
            final java.net.URI uri = java.net.URI.create(value.toString());
            return uri.isAbsolute();
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    /**
     * Transforms the specified value to an {@link RDFNode}, be it a resource or a literal.
     *
     * @param assertion Assertion representing the asserted property
     * @param value     Value to transform
     * @return Jena RDFNode
     */
    public static RDFNode valueToRdfNode(Assertion assertion, Value<?> value) {
        if (JenaUtils.isResourceIdentifier(value.getValue())) {
            return ResourceFactory.createResource(value.stringValue());
        } else {
            return assertion.hasLanguage() ?
                   ResourceFactory.createLangLiteral(value.stringValue(), assertion.getLanguage()) :
                   ResourceFactory.createTypedLiteral(value.getValue());
        }
    }

    public static Object literalToValue(Literal literal) {
        // This is because Jena returns XSD:long values as Integers, when they fit. But we don't want this.
        return literal.getDatatype().equals(XSDDatatype.XSDlong) ? literal.getLong() : literal.getValue();
    }
}
