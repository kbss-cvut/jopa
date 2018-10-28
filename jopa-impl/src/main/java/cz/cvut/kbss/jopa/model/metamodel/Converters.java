package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.oom.converter.*;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

class Converters {

    private final Map<Class<?>, ConverterWrapper<?, ?>> converters;

    Converters() {
        this.converters = new HashMap<>();
        addDefaultConverters();
    }

    private void addDefaultConverters() {
        converters.put(LocalDate.class, new LocalDateConverter());
        converters.put(LocalDateTime.class, new LocalDateTimeConverter());
        converters.put(Instant.class, new InstantConverter());
        converters.put(ZonedDateTime.class, new ZonedDateTimeConverter());
        converters.put(Short.class, new ToShortConverter());
        converters.put(Integer.class, new ToIntegerConverter());
        converters.put(Long.class, new ToLongConverter());
        converters.put(Float.class, new ToFloatConverter());
        converters.put(Double.class, new ToDoubleConverter());
        converters.put(String.class, new ToStringConverter());
    }

    Optional<ConverterWrapper<?, ?>> getConverter(Class<?> type) {
        return Optional.ofNullable(converters.get(type));
    }
}
