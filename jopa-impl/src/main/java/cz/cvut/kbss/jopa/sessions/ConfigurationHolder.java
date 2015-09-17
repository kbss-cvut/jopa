package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.utils.Configuration;

/**
 * Marker interface for classes providing access to the current configuration.
 */
public interface ConfigurationHolder {

    /**
     * Gets provider configuration.
     *
     * @return Configuration
     */
    Configuration getConfiguration();
}
