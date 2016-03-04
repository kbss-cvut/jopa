package cz.cvut.kbss.ontodriver.config;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Holds configuration of the OntoDriver.
 */
public final class Configuration {
    // TODO Tests

    private final ConcurrentHashMap<ConfigurationParameter, Object> configuration = new ConcurrentHashMap<>();

    /**
     * Loads configuration of parameters specified by {@code parameters} from the provided properties map.
     *
     * @param properties Map of configuration values
     * @param parameters Parameters to extract from the map
     */
    public void addConfiguration(Map<String, String> properties, Collection<ConfigurationParameter> parameters) {
        Objects.requireNonNull(properties);
        Objects.requireNonNull(parameters);
        parameters.stream().filter(p -> properties.containsKey(p.toString()))
                  .forEach(p -> setProperty(p, properties.get(p.toString())));
    }

    /**
     * Sets configuration of the specified parameter to the specified value.
     * <p>
     * Overrides any previous setting.
     *
     * @param property Parameter
     * @param value    Value
     */
    public void setProperty(ConfigurationParameter property, Object value) {
        Objects.requireNonNull(property);
        configuration.put(property, value);
    }

    /**
     * Gets value of the specified property.
     *
     * @param property Parameter
     * @return Value of the property or {@code null}, if it is not set
     */
    public Object getProperty(ConfigurationParameter property) {
        Objects.requireNonNull(property);
        return configuration.get(property);
    }

    /**
     * Gets value of the specified property or the default value, if the property is not set.
     *
     * @param property     Parameter
     * @param defaultValue Value to return if the property is not set
     * @return Value of the property or {@code defaultValue}, if it is not set
     */
    public Object getProperty(ConfigurationParameter property, Object defaultValue) {
        Objects.requireNonNull(property);
        return configuration.getOrDefault(property, defaultValue);
    }

    /**
     * Returns value of the specified property as the specified type (if it can be cast to it).
     *
     * @param property   Parameter
     * @param targetType Target type
     * @return Property value or {@code null}, if it is not set
     * @throws IllegalArgumentException if property value cannot be cast to {@code targetType}
     */
    public <T> T getProperty(ConfigurationParameter property, Class<T> targetType) {
        if (configuration.containsKey(property)) {
            final Object value = configuration.get(property);
            if (value != null && targetType.isAssignableFrom(value.getClass())) {
                return targetType.cast(value);
            } else {
                throw new IllegalArgumentException(
                        "Cannot return value " + value + " of property " + property + " as type " + targetType);
            }
        }
        return null;
    }

    /**
     * Returns value of the specified property as the specified type (if it can be cast to it).
     *
     * @param property     Parameter
     * @param targetType   Target type
     * @param defaultValue Value to return if the property is not set
     * @return Value of the property or {@code defaultValue}, if it is not set
     * @throws IllegalArgumentException if property value cannot be cast to {@code targetType}
     */
    public <T> T getProperty(ConfigurationParameter property, Class<T> targetType, T defaultValue) {
        final Object value = configuration.getOrDefault(property, defaultValue);
        if (value != null && targetType.isAssignableFrom(value.getClass())) {
            return targetType.cast(value);
        } else {
            throw new IllegalArgumentException(
                    "Cannot return value " + value + " of property " + property + " as type " + targetType);
        }
    }
}
