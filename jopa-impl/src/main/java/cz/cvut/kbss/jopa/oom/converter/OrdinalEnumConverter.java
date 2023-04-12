package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.exception.InvalidEnumMappingException;

/**
 * Built-in converter for mapping to/from enum-valued attributes.
 * <p>
 * This converter transforms enum constants to/from their ordinal number. It is used for enumerated attributes with
 * {@link cz.cvut.kbss.jopa.model.annotations.EnumType#ORDINAL} configuration.
 *
 * @param <E> Enum type
 */
public class OrdinalEnumConverter<E extends Enum<E>> implements ConverterWrapper<E, Object> {

    private final Class<E> enumType;

    public OrdinalEnumConverter(Class<E> enumType) {
        this.enumType = enumType;
    }

    @Override
    public Object convertToAxiomValue(E value) {
        assert value != null;
        return value.ordinal();
    }

    @Override
    public E convertToAttribute(Object value) {
        assert value instanceof Number;
        final int ordinal = ((Number) value).intValue();
        if (ordinal >= enumType.getEnumConstants().length) {
            throw new InvalidEnumMappingException("Value " + ordinal + " is not a valid ordinal in " + enumType);
        }
        return enumType.getEnumConstants()[ordinal];
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Integer.class.isAssignableFrom(type) || Short.class.isAssignableFrom(type) || Byte.class
                .isAssignableFrom(type);
    }
}
