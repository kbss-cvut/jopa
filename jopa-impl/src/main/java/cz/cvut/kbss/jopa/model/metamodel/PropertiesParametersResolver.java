package cz.cvut.kbss.jopa.model.metamodel;

import java.lang.reflect.*;

class PropertiesParametersResolver {

    private final java.lang.reflect.Type keyType;
    private final java.lang.reflect.Type valueType;

    public PropertiesParametersResolver(Field propertiesField) {
        final ParameterizedType typeSpec = (ParameterizedType) propertiesField.getGenericType();
        assert typeSpec.getActualTypeArguments().length == 2;
        this.keyType = typeSpec.getActualTypeArguments()[0];
        this.valueType = typeSpec.getActualTypeArguments()[1];
    }

    java.lang.reflect.Type getKeyType() {
        return keyType;
    }

    java.lang.reflect.Type getValueType() {
        return valueType;
    }

    Class<?> getPropertyIdentifierType() {
        assert keyType instanceof Class;
        return (Class<?>) keyType;
    }

    Class<?> getPropertyValueType() {
        assert valueType instanceof ParameterizedType;
        final ParameterizedType type = (ParameterizedType) valueType;
        assert type.getActualTypeArguments().length == 1;
        assert type.getActualTypeArguments()[0] instanceof Class;
        return (Class<?>) type.getActualTypeArguments()[0];
    }
}
