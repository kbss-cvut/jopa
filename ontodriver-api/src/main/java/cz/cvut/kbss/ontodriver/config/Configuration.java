/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.config;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Holds configuration of the OntoDriver.
 */
public final class Configuration {

    private final Map<ConfigurationParameter, String> configuration = new HashMap<>();

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
     * Returns value of the specified property as boolean.
     * <p>
     * If the property is not configured, {@code false} is returned.
     *
     * @param property Parameter
     * @return Property value (false for unknown)
     */
    public boolean is(ConfigurationParameter property) {
        if (configuration.containsKey(property)) {
            final String value = configuration.get(property);
            return Boolean.parseBoolean(value);
        }
        return false;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Configuration that = (Configuration) o;

        return configuration.equals(that.configuration);

    }

    @Override
    public int hashCode() {
        return configuration.hashCode();
    }
}
