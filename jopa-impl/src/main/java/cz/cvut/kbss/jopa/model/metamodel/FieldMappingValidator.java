/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.annotations.EnumType;
import cz.cvut.kbss.jopa.model.annotations.Enumerated;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
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
            throw new InvalidFieldMappingException(field, "@Properties must be of type " + Map.class.getName());
        }
        if (isRawType(field.getGenericType())) {
            throw new InvalidFieldMappingException(field, "@Properties field cannot be a raw map.");
        }
        final PropertiesParametersResolver parametersResolver = new PropertiesParametersResolver(field);
        if (!isValidIdentifierType(parametersResolver.getKeyType())) {
            throw new InvalidFieldMappingException(field,
                    "@Properties key type is not a valid identifier type. Expected one of " + IDENTIFIER_TYPES);
        }
        validatePropertiesValueType(field, parametersResolver.getValueType());
    }

    private static boolean isRawType(Type type) {
        return !(type instanceof ParameterizedType);
    }

    private static void validatePropertiesValueType(Field field, Type type) {
        if (isRawType(type)) {
            throw new InvalidFieldMappingException(field, "Properties value type cannot be raw");
        }
        if (!((ParameterizedType) type).getRawType().equals(Set.class)) {
            throw new InvalidFieldMappingException(field, "@Properties value type must be a " + Set.class.getName());
        }
    }

    void validateTypesField(Field field) {
        if (!Set.class.isAssignableFrom(field.getType())) {
            throw new InvalidFieldMappingException(field, "@Types must be of type " + Set.class.getName());
        }
        if (isRawType(field.getGenericType())) {
            throw new InvalidFieldMappingException(field, "@Types field cannot be raw");
        }
        final ParameterizedType typeSpec = (ParameterizedType) field.getGenericType();
        if (!isValidIdentifierType(typeSpec.getActualTypeArguments()[0])) {
            throw new InvalidFieldMappingException(field,
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
        switch (attribute.getPersistentAttributeType()) {
            case OBJECT:
                validateObjectPropertyEnumMapping(attribute);
                break;
            case DATA:  // Intentional fall-through
            case ANNOTATION:
                validateLexicalFormAttribute(attribute);
                validateSimpleLiteralField(attribute);
                validateNotSimpleList(attribute);
                break;
        }
    }

    private static void validateAttributeDoesNotMapRdfType(AbstractAttribute<?, ?> att) {
        if (RDF.TYPE.equals(att.getIRI().toString())) {
            throw new InvalidFieldMappingException(
                    att.getJavaField(),"cannot use rdf:type for property mapping. Use a Set field annotated with " + Types.class.getSimpleName());
        }
    }

    private static void validateLexicalFormAttribute(AbstractAttribute<?, ?> attribute) {
        if (attribute.isLexicalForm() && !String.class.isAssignableFrom(getBindableType(attribute))) {
            throw new InvalidFieldMappingException(
                    attribute.getJavaField(), "lexicalForm mapping can be used only on fields of type String.");
        }
    }

    private static void validateSimpleLiteralField(AbstractAttribute<?, ?> attribute) {
        final Class<?> fieldType = getBindableType(attribute);

        if (!attribute.isSimpleLiteral() || String.class.isAssignableFrom(fieldType) || Enum.class.isAssignableFrom(fieldType)) {
            return;
        }

        if(attribute.getConverter() != null && attribute.getConverter().supportsAxiomValueType(String.class)) {
            return;
        }

        throw new InvalidFieldMappingException(attribute.getJavaField(),"simpleLiteral mapping can only be used on fields of type String or Enum or using a suitable converter.");
    }

    private static Class<?> getBindableType(AbstractAttribute<?, ?> attribute) {
        return attribute.isCollection() ? ((AbstractPluralAttribute) attribute).getBindableJavaType() :
               attribute.getJavaType();
    }

    private static void validateObjectPropertyEnumMapping(Attribute<?, ?> attribute) {
        if (!attribute.getJavaType().isEnum()) {
            return;
        }
        final Enumerated enumeratedAnn = attribute.getJavaField().getAnnotation(Enumerated.class);
        if (enumeratedAnn == null || enumeratedAnn.value() != EnumType.OBJECT_ONE_OF) {
            throw new InvalidFieldMappingException(
                    "Attribute " + attribute + " maps an enum but is not annotated with " + Enumerated.class + " with " + EnumType.OBJECT_ONE_OF + " value.");
        }
    }

    private static void validateNotSimpleList(AbstractAttribute<?, ?> attribute) {
        if (attribute.isCollection() && ((PluralAttribute<?, ?, ?>) attribute).getCollectionType() == CollectionType.LIST) {
            final ListAttribute<?, ?> la = (ListAttribute<?, ?>) attribute;
            if (la.getSequenceType() == SequenceType.simple) {
                throw new InvalidFieldMappingException(attribute.getJavaField(), "simple list attribute must be mapped to an object property.");
            }
        }
    }
}
