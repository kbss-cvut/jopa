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

import cz.cvut.kbss.jopa.exception.InstantiationException;
import cz.cvut.kbss.jopa.exception.InvalidConverterException;
import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.AttributeConverter;
import cz.cvut.kbss.jopa.model.annotations.Convert;
import cz.cvut.kbss.jopa.oom.converter.*;
import cz.cvut.kbss.jopa.utils.ReflectionUtils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.Arrays;
import java.util.Optional;

/**
 * Determines potential converters which may be used on a field.
 * <p>
 * Currently, only built-in converters for data and annotation property attributes are supported, but in the future,
 * custom converters should be also supported.
 */
public class ConverterResolver {

    private final Converters converters;

    ConverterResolver(Converters converters) {
        this.converters = converters;
    }

    /**
     * Determines converter which should be used for transformation of values to and from the specified field.
     * <p>
     * Beside custom converters, the system supports a number of built-in converters, which ensure that e.g. widening
     * conversion or mapping to Java 8 Date/Time API is supported.
     * <p>
     * The order of priorities of converter resolution is:
     * <ol>
     *     <li>Custom converter declared on attribute with {@link Convert}</li>
     *     <li>Custom automatically applied converter declared with {@link cz.cvut.kbss.jopa.model.annotations.Converter}</li>
     *     <li>Built-in converter</li>
     * </ol>
     *
     * @param field  The field for which converter should be determined
     * @param config Mapping configuration extracted during metamodel building
     * @return Possible converter instance to be used for transformation of values of the specified field. Returns empty
     * {@code Optional} if no suitable converter is found (or needed)
     */
    public Optional<ConverterWrapper<?, ?>> resolveConverter(PropertyInfo field, PropertyAttributes config) {
        final Class<?> attValueType = config.getType().getJavaType();
        final Optional<ConverterWrapper<?, ?>> localCustomConverter = resolveCustomConverter(field, config);
        if (localCustomConverter.isPresent()) {
            return localCustomConverter;
        }
        final Optional<ConverterWrapper<?, ?>> globalCustomConverter = converters.getCustomConverter(attValueType);
        if (globalCustomConverter.isPresent()) {
            return globalCustomConverter;
        }
        if (attValueType.isEnum()) {
            return Optional.of(createEnumConverter(attValueType, config));
        }
        if (config.hasDatatype()) {
            verifyTypeIsString(field, attValueType);
            return Optional.of(new ToRdfLiteralConverter(config.getDatatype()));
        }
        if (config.isLexicalForm()) {
            return Optional.of(new ToLexicalFormConverter());
        }
        return Converters.getDefaultConverter(attValueType);
    }

    private static ConverterWrapper<?, ?> createEnumConverter(Class<?> valueType, PropertyAttributes pa) {
        switch (pa.getEnumType()) {
            case OBJECT_ONE_OF:
                return new ObjectOneOfEnumConverter(valueType);
            case ORDINAL:
                return new OrdinalEnumConverter(valueType);
            default:
                return new StringEnumConverter(valueType);
        }
    }

    private static void verifyTypeIsString(PropertyInfo field, Class<?> attValueType) {
        if (!attValueType.equals(String.class)) {
            throw new InvalidFieldMappingException(
                    "Attributes with explicit datatype identifier must have values of type String. " +
                            "The provided attribute " + field + " has type " + attValueType);
        }
    }

    private static Optional<ConverterWrapper<?, ?>> resolveCustomConverter(PropertyInfo field, PropertyAttributes config) {
        final Convert convertAnn = field.getAnnotation(Convert.class);
        if (convertAnn == null || convertAnn.disableConversion()) {
            return Optional.empty();
        }
        if (config.getPersistentAttributeType() == Attribute.PersistentAttributeType.OBJECT) {
            throw new InvalidConverterException("Attribute converters cannot be declared attributes mapped to object properties.");
        }
        return Optional.of(createCustomConverter(convertAnn.converter()));
    }

    public static ConverterWrapper<?, ?> createCustomConverter(Class<?> converterType) {
        if (!AttributeConverter.class.isAssignableFrom(converterType)) {
            throw new InvalidConverterException(
                    "Specified converter type " + converterType + " does not implement " + AttributeConverter.class);
        }
        try {
            final AttributeConverter<?, ?> converter =
                    (AttributeConverter<?, ?>) ReflectionUtils.instantiateUsingDefaultConstructor(converterType);
            return new CustomConverterWrapper(converter, resolveConverterAxiomType(converterType));
        } catch (InstantiationException e) {
            throw new InvalidConverterException("Unable to instantiate attribute converter.", e);
        }
    }

    public static Class<?> resolveConverterAttributeType(Class<?> converterType) {
        final ParameterizedType typeSpec = getConverterGenerics(converterType);
        assert typeSpec.getActualTypeArguments().length == 2;
        return (Class<?>) typeSpec.getActualTypeArguments()[0];
    }

    private static ParameterizedType getConverterGenerics(Class<?> converterType) {
        return (ParameterizedType) Arrays.stream(converterType.getGenericInterfaces())
                                         .filter(t -> t instanceof ParameterizedType && ((ParameterizedType) t).getRawType()
                                                                                                               .equals(AttributeConverter.class))
                                         .findAny().orElseThrow(
                        () -> new InvalidConverterException(
                                "Specified converter type " + converterType + " does not implement " + AttributeConverter.class.getSimpleName()));
    }

    private static Class<?> resolveConverterAxiomType(Class<?> converterType) {
        final ParameterizedType typeSpec = getConverterGenerics(converterType);
        assert typeSpec.getActualTypeArguments().length == 2;
        return (Class<?>) typeSpec.getActualTypeArguments()[1];

    }

    /**
     * Alternative method for resolving converter. Can be used when the persistent attribute type is not relevant and/or
     * the class {@link PropertyAttributes} is not used for attribute configuration, e.g. for attributes defined by a
     * query.
     * <p>
     * Determines converter which should be used for transformation of values to and from the specified type of field.
     * <p>
     * Beside custom converters, the system supports a number of built-in converters, which ensure that e.g. widening
     * conversion or mapping to Java 8 Date/Time API is supported.
     *
     * @param type attribute type as defined in {@link cz.cvut.kbss.jopa.model.metamodel.Type} (not to be confused with
     *             {@link java.lang.reflect.Type})
     * @return Possible converter instance to be used for transformation of values of the specified field. Returns empty
     * {@code Optional} if no suitable converter is found (or needed)
     * @see cz.cvut.kbss.jopa.model.metamodel.QueryAttribute
     */
    public Optional<ConverterWrapper<?, ?>> resolveConverter(Type<?> type) {
        final Class<?> attValueType = type.getJavaType();
        if (attValueType.isEnum()) {
            return Optional.of(new StringEnumConverter(attValueType));
        }

        return converters.getCustomConverter(attValueType);
    }

    /**
     * Registers the specified converter for automatically converting values to the specified attribute type.
     *
     * @param attributeType Entity attribute type to convert to/from
     * @param converter     Converter instance
     */
    public void registerConverter(Class<?> attributeType, ConverterWrapper<?, ?> converter) {
        converters.registerConverter(attributeType, converter);
    }
}
