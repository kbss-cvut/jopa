/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.EnumConverter;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.oom.converter.ToRdfLiteralConverter;

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
     * Besides custom converters, the system supports a number of built-in converters, which ensure that e.g. widening
     * conversion or mapping to Java 8 Date/Time API is supported.
     *
     * @param field  The field for which converter should be determined
     * @param config Mapping configuration extracted during metamodel building
     * @return Possible converter instance to be used for transformation of values of the specified field. Returns empty
     * {@code Optional} if no suitable converter is found (or needed)
     */
    public Optional<ConverterWrapper<?, ?>> resolveConverter(Field field, PropertyAttributes config) {
        if (config.getPersistentAttributeType() == Attribute.PersistentAttributeType.OBJECT) {
            return Optional.empty();
        }
        final Class<?> attValueType = config.getType().getJavaType();
        if (attValueType.isEnum()) {
            return Optional.of(new EnumConverter(attValueType));
        }
        if (config.hasDatatype()) {
            return Optional.of(new ToRdfLiteralConverter(config.getDatatype()));
        }
        if (config.isLexicalForm()) {
            return Optional.of(new ToLexicalFormConverter());
        }
        return converters.getConverter(attValueType);
    }

    /**
     * Alternative method for resolving converter. Can be used when the persistent attribute type is not relevant
     * and/or the class {@link PropertyAttributes} is not used for attribute configuration,
     * e.g. for attributes defined by a query.
     * <p>
     * Determines converter which should be used for transformation of values to and from the specified type of a field.
     * <p>
     * Besides custom converters, the system supports a number of built-in converters, which ensure that e.g. widening
     * conversion or mapping to Java 8 Date/Time API is supported.
     *
     * @param type attribute type as defined in {@link cz.cvut.kbss.jopa.model.metamodel.Type}
     *             (not to be confused with {@link java.lang.reflect.Type})
     * @return Possible converter instance to be used for transformation of values of the specified field. Returns empty
     *         {@code Optional} if no suitable converter is found (or needed)
     * @see cz.cvut.kbss.jopa.model.metamodel.QueryAttribute
     */
    public Optional<ConverterWrapper<?, ?>> resolveConverter(Type<?> type) {
        final Class<?> attValueType = type.getJavaType();
        if (attValueType.isEnum()) {
            return Optional.of(new EnumConverter(attValueType));
        }

        return converters.getConverter(attValueType);
    }
}
