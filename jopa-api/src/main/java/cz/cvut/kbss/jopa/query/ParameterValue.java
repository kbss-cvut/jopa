package cz.cvut.kbss.jopa.query;

import cz.cvut.kbss.jopa.query.impl.LiteralParameterValue;
import cz.cvut.kbss.jopa.query.impl.UriParameterValue;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

/**
 * Query parameter value holder.
 *
 * @author kidney
 */
public abstract class ParameterValue {

    public abstract String getValue();

    public static ParameterValue create(Object value) {
        if (value instanceof URI) {
            return new UriParameterValue((URI) value);
        } else if (value instanceof URL) {
            try {
                return new UriParameterValue(((URL) value).toURI());
            } catch (URISyntaxException e) {
                throw new IllegalArgumentException("Unable to transform the specified URL to URI.", e);
            }
        } else {
            return new LiteralParameterValue(value);
        }
    }
}
