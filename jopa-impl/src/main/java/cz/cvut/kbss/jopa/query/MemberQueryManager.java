package cz.cvut.kbss.jopa.query;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class MemberQueryManager {

    private final Map<Method, String> methodQueryMap = new HashMap<>();
    private final Map<Field, String> fieldQueryMap = new HashMap<>();

    public void addMethodQuery(Method method, String query) {
        Objects.requireNonNull(method);
        Objects.requireNonNull(query);

        if (methodQueryMap.containsKey(method)) {
            throw new IllegalArgumentException("Query on method " + method.getName() + " already exists in this persistence unit.");
        }
        methodQueryMap.put(method, query);
    }

    public void addFieldQuery(Field field, String query) {
        Objects.requireNonNull(field);
        Objects.requireNonNull(query);

        if (fieldQueryMap.containsKey(field)) {
            throw new IllegalArgumentException("Query on field " + field.getName() + " already exists in this persistence unit.");
        }
        fieldQueryMap.put(field, query);
    }

    public String getMethodQuery(Method method) {
        if (!methodQueryMap.containsKey(method)) {
            throw new IllegalArgumentException("Query on method " + method.getName() + " was not found in this persistence unit.");
        }
        return methodQueryMap.get(method);
    }

    public String getFieldQuery(Field field) {
        if (!fieldQueryMap.containsKey(field)) {
            throw new IllegalArgumentException("Query on method " + field.getName() + " was not found in this persistence unit.");
        }
        return fieldQueryMap.get(field);
    }
}
