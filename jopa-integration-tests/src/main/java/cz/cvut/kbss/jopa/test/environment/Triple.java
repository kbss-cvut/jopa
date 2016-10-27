package cz.cvut.kbss.jopa.test.environment;

import java.net.URI;

public class Triple {

    private URI subject;
    private URI property;
    private Object value;

    public Triple(URI subject, URI property, Object value) {
        this.subject = subject;
        this.property = property;
        this.value = value;
    }

    public URI getSubject() {
        return subject;
    }

    public URI getProperty() {
        return property;
    }

    public Object getValue() {
        return value;
    }
}
