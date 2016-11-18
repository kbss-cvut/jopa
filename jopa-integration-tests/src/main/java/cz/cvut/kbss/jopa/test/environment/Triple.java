package cz.cvut.kbss.jopa.test.environment;

import java.net.URI;
import java.util.Objects;

public class Triple {

    private URI subject;
    private URI property;
    private Object value;

    public Triple(URI subject, URI property, Object value) {
        this.subject = Objects.requireNonNull(subject);
        this.property = Objects.requireNonNull(property);
        this.value = Objects.requireNonNull(value);
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Triple)) return false;

        Triple triple = (Triple) o;

        if (!subject.equals(triple.subject)) return false;
        if (!property.equals(triple.property)) return false;
        return value.equals(triple.value);

    }

    @Override
    public int hashCode() {
        int result = subject.hashCode();
        result = 31 * result + property.hashCode();
        result = 31 * result + value.hashCode();
        return result;
    }
}
