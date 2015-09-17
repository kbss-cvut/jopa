package cz.cvut.kbss.jopa.utils;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Contains configuration for the persistence provider and utility access methods for it.
 * <p>
 * This class is thread safe.
 */
public final class Configuration {

    private final Map<String, String> properties;

    public Configuration(Map<String, String> properties) {
        Objects.requireNonNull(properties);
        this.properties = new HashMap<>(properties);
    }

    public String get(String property) {
        return get(property, null);
    }

    public String get(String property, String defaultValue) {
        return properties.getOrDefault(property, defaultValue);
    }

    public boolean is(String property) {
        final String propertyValue = get(property, null);
        return propertyValue != null && Boolean.parseBoolean(propertyValue);
    }

    public synchronized void set(String property, String value) {
        Objects.requireNonNull(property);
        properties.put(property, value);
    }

    public boolean contains(String property) {
        return properties.containsKey(property);
    }

    public Map<String, String> getProperties() {
        return Collections.unmodifiableMap(properties);
    }
}
