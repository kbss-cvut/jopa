package cz.cvut.kbss.jopa.environment.utils;

import java.net.URI;

/**
 * Marker interface for entities with URI identifier.
 */
public interface HasUri {

    URI getUri();

    void setUri(URI uri);
}
