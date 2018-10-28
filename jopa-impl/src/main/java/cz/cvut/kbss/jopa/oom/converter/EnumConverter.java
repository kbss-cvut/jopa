package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.exception.UnsupportedTypeTransformation;

/**
 * Built-in converter for mapping to/from enum-valued attributes.
 * <p>
 * Uses the default {@link Enum#valueOf(Class, String)} method to transform string-based value loaded from repository to the appropriate
 * attribute type. In the opposite direction, enum value is converted to {@code String}.
 *
 * @param <E>
 */
public class EnumConverter<E extends Enum<E>> implements ConverterWrapper<E, Object> {

    private final Class<E> enumType;

    public EnumConverter(Class<E> enumType) {
        this.enumType = enumType;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
    }

    @Override
    public Object convertToAxiomValue(E value) {
        return value.toString();
    }

    @Override
    public E convertToAttribute(Object value) {
        try {
            return Enum.valueOf(enumType, value.toString());
        } catch (IllegalArgumentException e) {
            throw new UnsupportedTypeTransformation("Unable to transform value " + value + " to enum " + enumType, e);
        }
    }
}
