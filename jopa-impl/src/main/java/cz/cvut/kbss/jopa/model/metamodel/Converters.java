/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.oom.converter.*;
import cz.cvut.kbss.jopa.oom.converter.datetime.*;
import cz.cvut.kbss.jopa.utils.Configuration;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.*;

/**
 * Manages attribute converters.
 */
public class Converters {

    private static final Map<Class<?>, ConverterWrapper<?, ?>> DEFAULT_CONVERTERS = initDefaultConverters();

    private final Map<Class<?>, ConverterWrapper<?, ?>> converters = new HashMap<>();

    Converters(Configuration configuration) {
        initConverters(configuration);
    }

    private void initConverters(Configuration configuration) {
        converters.put(Object.class,
                new ObjectConverter(configuration.is(JOPAPersistenceProperties.PREFER_MULTILINGUAL_STRING)));
    }

    Optional<ConverterWrapper<?, ?>> getCustomConverter(Class<?> attributeType) {
        return Optional.ofNullable(converters.get(attributeType));
    }

    void registerConverter(Class<?> attributeType, ConverterWrapper<?, ?> converter) {
        converters.put(attributeType, converter);
    }

    public static Optional<ConverterWrapper<?, ?>> getDefaultConverter(Class<?> attributeType) {
        return Optional.ofNullable(DEFAULT_CONVERTERS.get(attributeType));
    }

    private static Map<Class<?>, ConverterWrapper<?, ?>> initDefaultConverters() {
        final Map<Class<?>, ConverterWrapper<?, ?>> converters = new HashMap<>();
        converters.put(LocalDateTime.class, new LocalDateTimeConverter());
        converters.put(LocalTime.class, new LocalTimeConverter());
        converters.put(Instant.class, new InstantConverter());
        converters.put(ZonedDateTime.class, new ZonedDateTimeConverter());
        converters.put(Date.class, new DateConverter());
        converters.put(Short.class, new ToShortConverter());
        converters.put(Integer.class, new ToIntegerConverter());
        converters.put(Long.class, new ToLongConverter());
        converters.put(Float.class, new ToFloatConverter());
        converters.put(Double.class, new ToDoubleConverter());
        converters.put(String.class, new ToStringConverter());
        return Collections.unmodifiableMap(converters);
    }

    /**
     * Gets a map of default, built-in converters.
     *
     * @return Map of built-in converters, where the key is the target attribute type
     */
    public static Map<Class<?>, ConverterWrapper<?, ?>> getDefaultConverters() {
        return DEFAULT_CONVERTERS;
    }
}
