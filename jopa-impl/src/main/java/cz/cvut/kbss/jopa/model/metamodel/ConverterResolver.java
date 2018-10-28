package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.lang.reflect.Field;
import java.util.Optional;

/**
 * Determines potential converters which may be used on a field.
 * <p>
 * Currently, only built-in converters for data and annotation property attributes are supported, but in the future,
 * custom converters should be also supported.
 */
class ConverterResolver {

    private final Converters converters;

    ConverterResolver(Converters converters) {
        this.converters = converters;
    }

    /**
     * Determines converter which should be used for transformation of values to and from the specified field.
     * <p>
     * Besides custom converters, the system supports a number of built-in converters, which ensure that e.g. widening conversion
     * or mapping to Java 8 Date/Time API is supported.
     *
     * @param field  The field for which converter should be determined
     * @param config Mapping configuration extracted during metamodel building
     * @return Possible converter instance to be used for transformation of values of the specified field.
     * Returns empty {@code Optional} if no suitable converter is found (or needed)
     */
    public Optional<ConverterWrapper<?, ?>> resolveConverter(Field field, PropertyAttributes config) {
        if (config.getPersistentAttributeType() == Attribute.PersistentAttributeType.OBJECT) {
            return Optional.empty();
        }
        final Class<?> attValueType = config.getType().getJavaType();
        return converters.getConverter(attValueType);
    }
}
