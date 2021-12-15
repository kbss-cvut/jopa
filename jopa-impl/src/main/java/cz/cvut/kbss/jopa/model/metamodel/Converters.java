/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.oom.converter.datetime.DateConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.InstantConverter;
import cz.cvut.kbss.jopa.utils.Configuration;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

class Converters {

    private final Map<Class<?>, ConverterWrapper<?, ?>> defaultConverters;

    Converters(Configuration configuration) {
        this.defaultConverters = new HashMap<>();
        addDefaultConverters(configuration);
    }

    private void addDefaultConverters(Configuration configuration) {
        defaultConverters.put(LocalDate.class, new LocalDateConverter());
        defaultConverters.put(LocalDateTime.class, new LocalDateTimeConverter());
        defaultConverters.put(Instant.class, new InstantConverter());
        defaultConverters.put(ZonedDateTime.class, new ZonedDateTimeConverter());
        defaultConverters.put(Date.class, new DateConverter());
        defaultConverters.put(Short.class, new ToShortConverter());
        defaultConverters.put(Integer.class, new ToIntegerConverter());
        defaultConverters.put(Long.class, new ToLongConverter());
        defaultConverters.put(Float.class, new ToFloatConverter());
        defaultConverters.put(Double.class, new ToDoubleConverter());
        defaultConverters.put(Object.class,
                new ObjectConverter(configuration.is(JOPAPersistenceProperties.PREFER_MULTILINGUAL_STRING)));
        defaultConverters.put(String.class, new ToStringConverter());
    }

    Optional<ConverterWrapper<?, ?>> getConverter(Class<?> type) {
        return Optional.ofNullable(defaultConverters.get(type));
    }
}
