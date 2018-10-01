package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.AttributeConverter;
import cz.cvut.kbss.jopa.oom.converter.*;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Map;

class Converters {

    private final Map<Class<?>, AttributeConverter<?, ?>> converters;

    Converters() {
        this.converters = new HashMap<>();
        addDefaultConverters();
    }

    private void addDefaultConverters() {
        converters.put(LocalDate.class, new LocalDateConverter());
        converters.put(LocalDateTime.class, new LocalDateTimeConverter());
        converters.put(Instant.class, new InstantConverter());
        converters.put(ZonedDateTime.class, new ZonedDateTimeConverter());
    }

    AttributeConverter<?, ?> getConverter(Class<?> type) {
        return converters.getOrDefault(type, DefaultConverter.INSTANCE);
    }
}
