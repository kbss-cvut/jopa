package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.model.NamedResource;

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
}
