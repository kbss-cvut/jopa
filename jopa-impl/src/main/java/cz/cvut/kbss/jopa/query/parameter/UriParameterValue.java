package cz.cvut.kbss.jopa.query.parameter;

import java.net.URI;

/**
 * Parameter value that will be put as an IRI into the query. I.e. it will be enclosed in < and >.
 */
class UriParameterValue extends ParameterValue {

    private final URI uri;

    public UriParameterValue(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getValue() {
        return uri;
    }

    @Override
    public String getQueryString() {
        return "<" + uri.toString() + ">";
    }

    @Override
    public String toString() {
        return getQueryString();
    }
}
