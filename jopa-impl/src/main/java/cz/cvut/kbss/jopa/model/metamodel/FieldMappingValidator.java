package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;

import java.lang.reflect.*;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.*;

/**
 * Verifies that a field's mapping metadata and declaration are valid.
 */
class FieldMappingValidator {

    private static final Set<Type> VALID_ID_TYPES = initIdClasses();

    private static Set<Type> initIdClasses() {
        final Set<Type> set = new HashSet<>(4);
        set.add(String.class);
        set.add(URI.class);
        return set;
    }

    void validatePropertiesField(Field field) {
        assert field != null;
        if (!Map.class.isAssignableFrom(field.getType())) {
            throw new InvalidFieldMappingException(
                    "Expected @Properties field to be a map, but it is a " + field.getType());
        }
        if (isRawType(field.getGenericType())) {
            throw new InvalidFieldMappingException("@Properties field cannot be a raw map.");
        }
        final ParameterizedType typeSpec = (ParameterizedType) field.getGenericType();
        assert typeSpec.getActualTypeArguments().length == 2;
        if (!isValidIdentifierType(typeSpec.getActualTypeArguments()[0])) {
            throw new InvalidFieldMappingException(
                    "@Properties key type is not a valid identifier type. Expected one of " + VALID_ID_TYPES);
        }
        validatePropertiesValueType(typeSpec.getActualTypeArguments()[1]);
    }

    private boolean isRawType(Type type) {
        return !(type instanceof ParameterizedType);
    }

    private void validatePropertiesValueType(Type type) {
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
                    "@Types field value is not a valid identifier type. Expected one of " + VALID_ID_TYPES);
        }
    }

    boolean isValidIdentifierType(Type type) {
        return VALID_ID_TYPES.contains(type);
    }
}
