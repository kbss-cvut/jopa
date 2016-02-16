package cz.cvut.kbss.ontodriver.model;

import java.io.Serializable;
import java.net.URI;
import java.util.Objects;

/**
 * Represents named resources, i. e. resources identified by an URI.
 *
 * @author ledvima1
 */
public class NamedResource implements Serializable {

    private static final long serialVersionUID = 5932515448919851871L;

    private final URI identifier;

    NamedResource(URI uri) {
        this.identifier = Objects.requireNonNull(uri);
    }

    /**
     * Gets identifier of this resource. </p>
     *
     * @return URI
     */
    public URI getIdentifier() {
        return identifier;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + identifier.hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        NamedResource other = (NamedResource) obj;
        if (!identifier.equals(other.identifier))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return identifier.toString();
    }

    /**
     * Creates new named resource from the specified URI.
     *
     * @param uri Resource identifier
     * @return NamedResource instance
     */
    public static NamedResource create(URI uri) {
        return new NamedResource(uri);
    }

    /**
     * Creates new named resource from the specified string identifier.
     *
     * @param iri Resource identifier
     * @return NamedResource instance
     */
    public static NamedResource create(String iri) {
        return new NamedResource(URI.create(iri));
    }
}
