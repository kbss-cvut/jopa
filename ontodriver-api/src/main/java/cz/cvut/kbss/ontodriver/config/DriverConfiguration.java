/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.config;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Holds configuration of the OntoDriver.
 */
public final class DriverConfiguration {

    private final Map<ConfigurationParameter, String> configuration = new HashMap<>();
    private final OntologyStorageProperties storageProperties;

    public DriverConfiguration(OntologyStorageProperties storageProperties) {
        this.storageProperties = Objects.requireNonNull(storageProperties);
    }

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
    public void setProperty(ConfigurationParameter property, String value) {
        Objects.requireNonNull(property);
        configuration.put(property, value);
    }

    /**
     * Gets value of the specified property.
     *
     * @param property Parameter
     * @return Value of the property or {@code null}, if it is not set
     */
    public String getProperty(ConfigurationParameter property) {
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
    public String getProperty(ConfigurationParameter property, String defaultValue) {
        Objects.requireNonNull(property);
        return configuration.getOrDefault(property, defaultValue);
    }

    /**
     * Gets integer value of the specified property or the default value, if the property is not set.
     *
     * @param property     Parameter
     * @param defaultValue Value to return if the property is not set
     * @return Value of the property or {@code defaultValue}, if it is not set
     * @throws IllegalArgumentException If the configured property value is not an integer
     */
    public int getProperty(ConfigurationParameter property, int defaultValue) {
        final String propertyValue = getProperty(property, Integer.toString(defaultValue));
        try {
            return Integer.parseInt(propertyValue);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Value '" + propertyValue + "' of property '" + propertyValue + "' is not a valid integer.", e);
        }
    }

    /**
     * Returns value of the specified property as boolean.
     * <p>
     * If the property is not configured, {@code false} is returned.
     *
     * @param property Parameter
     * @return Property value (false for unknown)
     */
    public boolean is(ConfigurationParameter property) {
        Objects.requireNonNull(property);
        return Boolean.parseBoolean(configuration.get(property));
    }

    /**
     * Checks whether the specified property is set in this configuration.
     *
     * @param property The property to check
     * @return Whether the specified property is set here
     */
    public boolean isSet(ConfigurationParameter property) {
        return configuration.containsKey(property);
    }

    public OntologyStorageProperties getStorageProperties() {
        return storageProperties;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        DriverConfiguration that = (DriverConfiguration) o;

        return configuration.equals(that.configuration) && storageProperties.equals(that.storageProperties);

    }

    @Override
    public int hashCode() {
        int result = configuration.hashCode();
        result = 31 * result + storageProperties.hashCode();
        return result;
    }
}
