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

    private final Map<Class<?>, ConverterWrapper<?, ?>> defaultConverters;

    Converters() {
        this.defaultConverters = new HashMap<>();
        addDefaultConverters();
    }

    private void addDefaultConverters() {
        defaultConverters.put(LocalDate.class, new LocalDateConverter());
        defaultConverters.put(LocalDateTime.class, new LocalDateTimeConverter());
        defaultConverters.put(Instant.class, new InstantConverter());
        defaultConverters.put(ZonedDateTime.class, new ZonedDateTimeConverter());
        defaultConverters.put(Short.class, new ToShortConverter());
        defaultConverters.put(Integer.class, new ToIntegerConverter());
        defaultConverters.put(Long.class, new ToLongConverter());
        defaultConverters.put(Float.class, new ToFloatConverter());
        defaultConverters.put(Double.class, new ToDoubleConverter());
    }

    Optional<ConverterWrapper<?, ?>> getConverter(Class<?> type) {
        return Optional.ofNullable(defaultConverters.get(type));
    }
}
