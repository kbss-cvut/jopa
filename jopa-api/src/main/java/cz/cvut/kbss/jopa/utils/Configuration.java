/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
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

    public String get(String property) {
        return get(property, null);
    }

    public String get(String property, String defaultValue) {
        return properties.getOrDefault(property, defaultValue);
    }

    public boolean is(String property) {
        final String propertyValue = get(property, null);
        return Boolean.parseBoolean(propertyValue);
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
