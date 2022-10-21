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

import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.jopa.vocabulary.RDF;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.Set;

import static cz.cvut.kbss.jopa.model.PersistenceProperties.IDENTIFIER_TYPES;

/**
 * Verifies that a field's mapping metadata and declaration are valid.
 */
class FieldMappingValidator {

    void validatePropertiesField(Field field) {
        assert field != null;
        if (!Map.class.isAssignableFrom(field.getType())) {
            throw new InvalidFieldMappingException(
                    "Expected @Properties field to be a map, but it is a " + field.getType());
        }
        if (isRawType(field.getGenericType())) {
            throw new InvalidFieldMappingException("@Properties field cannot be a raw map.");
        }
        final PropertiesParametersResolver parametersResolver = new PropertiesParametersResolver(field);
        if (!isValidIdentifierType(parametersResolver.getKeyType())) {
            throw new InvalidFieldMappingException(
                    "@Properties key type is not a valid identifier type. Expected one of " + IDENTIFIER_TYPES);
        }
        validatePropertiesValueType(parametersResolver.getValueType());
    }

    private static boolean isRawType(Type type) {
        return !(type instanceof ParameterizedType);
    }

    private static void validatePropertiesValueType(Type type) {
        if (isRawType(type)) {
            throw new InvalidFieldMappingException(
                    "Expected a java.util.Set as value parameter of the @Properties map, but got " + type);
        }
        if (!((ParameterizedType) type).getRawType().equals(Set.class)) {
            throw new InvalidFieldMappingException(
                    "Expected a java.util.Set as value parameter of the @Properties map, but got " + type);
        }
    }

    void validateTypesField(Field field) {
        if (!Set.class.isAssignableFrom(field.getType())) {
            throw new InvalidFieldMappingException("Expected @Types field to be a set, but it is a " + field.getType());
        }
        if (isRawType(field.getGenericType())) {
            throw new InvalidFieldMappingException("@Types field cannot be a raw set.");
        }
        final ParameterizedType typeSpec = (ParameterizedType) field.getGenericType();
        if (!isValidIdentifierType(typeSpec.getActualTypeArguments()[0])) {
            throw new InvalidFieldMappingException(
                    "@Types field value is not a valid identifier type. Expected one of " + IDENTIFIER_TYPES);
        }
    }

    void validateIdentifierType(Type type) {
        if (!isValidIdentifierType(type)) {
            throw new InvalidFieldMappingException(type + " is not a valid identifier type.");
        }
    }

    boolean isValidIdentifierType(Type type) {
        return type instanceof Class && IdentifierTransformer.isValidIdentifierType((Class<?>) type);
    }

    void validateAttributeMapping(AbstractAttribute<?, ?> attribute) {
        validateAttributeDoesNotMapRdfType(attribute);
        if (attribute.getPersistentAttributeType() == Attribute.PersistentAttributeType.DATA
                || attribute.getPersistentAttributeType() == Attribute.PersistentAttributeType.ANNOTATION) {
            validateLexicalFormAttribute(attribute);
            validateSimpleLiteralField(attribute);
        }
    }

    private static void validateAttributeDoesNotMapRdfType(AbstractAttribute<?, ?> att) {
        if (RDF.TYPE.equals(att.getIRI().toString())) {
            throw new InvalidFieldMappingException(
                    att + " - cannot use rdf:type for property mapping. Use a Set field annotated with " + Types.class.getSimpleName());
        }
    }

    private static void validateLexicalFormAttribute(AbstractAttribute<?, ?> attribute) {
        if (attribute.isLexicalForm() && !String.class.isAssignableFrom(getLiteralFieldType(attribute))) {
            throw new InvalidFieldMappingException(
                    attribute + " - lexicalForm mapping can be used only on fields of type String.");
        }
    }

    private static void validateSimpleLiteralField(AbstractAttribute<?, ?> attribute) {
        final Class<?> fieldType = getLiteralFieldType(attribute);
        if (attribute.isSimpleLiteral() && (!String.class.isAssignableFrom(fieldType) && !Enum.class.isAssignableFrom(
                fieldType) && !attribute.getConverter().supportsAxiomValueType(String.class))) {
            throw new InvalidFieldMappingException(
                    attribute + " - simpleLiteral mapping can only be used on fields of type String or Enum or using a suitable converter.");
        }
    }

    private static Class<?> getLiteralFieldType(AbstractAttribute<?, ?> attribute) {
        return attribute.isCollection() ? ((AbstractPluralAttribute) attribute).getBindableJavaType() : attribute.getJavaType();
    }
}
