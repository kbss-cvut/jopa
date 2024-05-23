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

    public Configuration() {
        this.properties = new HashMap<>();
    }

    public Configuration(Map<String, String> properties) {
        Objects.requireNonNull(properties);
        this.properties = new HashMap<>(properties);
    }

    /**
     * Gets the configured value of the specified property.
     *
     * @param property Property whose value to get
     * @return Configured value, {@code null} if property is not configured
     */
    public String get(String property) {
        return get(property, null);
    }

    /**
     * Gets the configured value of the specified property.
     * <p>
     * The specified default value is returned if the property is not configured.
     *
     * @param property     Property whose value to get
     * @param defaultValue Default value to return if no value is configured
     * @return Configured value, {@code defaultValue} if property is not configured
     */
    public String get(String property, String defaultValue) {
        return properties.getOrDefault(property, defaultValue);
    }

    /**
     * Gets the configured value of the specified boolean-valued property.
     *
     * @param property Property whose value to get
     * @return {@code true} if the specified property is configured and its value is the boolean true, {@code false}
     * otherwise
     */
    public boolean is(String property) {
        final String propertyValue = get(property, null);
        return Boolean.parseBoolean(propertyValue);
    }

    /**
     * Sets the specified value for the specified property.
     *
     * @param property Property whose value to set
     * @param value    Value to set
     */
    public synchronized void set(String property, String value) {
        Objects.requireNonNull(property);
        properties.put(property, value);
    }

    /**
     * Checks whether this configuration contains an explicitly set value of the specified property.
     *
     * @param property Property whose presence to check
     * @return {@code true} if the specified property is configured, {@code false} otherwise
     */
    public boolean contains(String property) {
        return properties.containsKey(property);
    }

    /**
     * Gets an unmodifiable view of the underlying map of configuration.
     *
     * @return Unmodifiable Map
     */
    public Map<String, String> getProperties() {
        return Collections.unmodifiableMap(properties);
    }
}
