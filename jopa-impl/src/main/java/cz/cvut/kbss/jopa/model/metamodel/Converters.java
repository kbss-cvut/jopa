/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.ObjectConverter;
import cz.cvut.kbss.jopa.oom.converter.ToDoubleConverter;
import cz.cvut.kbss.jopa.oom.converter.ToFloatConverter;
import cz.cvut.kbss.jopa.oom.converter.ToIntegerConverter;
import cz.cvut.kbss.jopa.oom.converter.ToLangStringConverter;
import cz.cvut.kbss.jopa.oom.converter.ToLongConverter;
import cz.cvut.kbss.jopa.oom.converter.ToShortConverter;
import cz.cvut.kbss.jopa.oom.converter.ToStringConverter;
import cz.cvut.kbss.jopa.oom.converter.ToURIConverter;
import cz.cvut.kbss.jopa.oom.converter.ToURLConverter;
import cz.cvut.kbss.jopa.oom.converter.CharacterConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.DateConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.InstantConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.LocalDateTimeConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.LocalTimeConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.ZonedDateTimeConverter;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.model.LangString;

import java.net.URI;
import java.net.URL;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

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
        converters.put(Object.class, new ObjectConverter(configuration.is(JOPAPersistenceProperties.PREFER_MULTILINGUAL_STRING)));
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
        return Map.ofEntries(Map.entry(LocalDateTime.class, new LocalDateTimeConverter()),
                Map.entry(LocalTime.class, new LocalTimeConverter()),
                Map.entry(Instant.class, new InstantConverter()),
                Map.entry(ZonedDateTime.class, new ZonedDateTimeConverter()),
                Map.entry(Date.class, new DateConverter()),
                Map.entry(Short.class, new ToShortConverter()),
                Map.entry(Integer.class, new ToIntegerConverter()),
                Map.entry(Long.class, new ToLongConverter()),
                Map.entry(Float.class, new ToFloatConverter()),
                Map.entry(Double.class, new ToDoubleConverter()),
                Map.entry(String.class, new ToStringConverter()),
                Map.entry(LangString.class, new ToLangStringConverter()),
                Map.entry(URI.class, new ToURIConverter()),
                Map.entry(URL.class, new ToURLConverter()),
                Map.entry(Character.class, new CharacterConverter()));
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
