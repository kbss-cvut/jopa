package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;

import java.beans.Introspector;
import java.lang.reflect.Method;

public abstract class AnnotatedAccessor {
    protected final Method method;
    protected final String propertyName;
    protected final Class<?> propertyType;

    protected AnnotatedAccessor(Method method) {
        this.method = method;
        this.propertyName = extractPropertyNameFromMethod(method);
        this.propertyType = extractPropertyTypeNameFromMethod(method);
    }

    public static AnnotatedAccessor from(Method method) {
        if (isSetter(method)) {
            return new AnnotatedSetter(method);
        } else if (isGetter(method)) {
            return new AnnotatedGetter(method);
        } else {
            throw new MetamodelInitializationException("Method {} is neither a setter or getter. Only simple getters and setters following the Java naming convection can be used for OWL property annotations.");
        }

    }

    protected abstract String extractPropertyNameFromMethod(Method m);

    protected abstract Class<?> extractPropertyTypeNameFromMethod(Method m);

    private static boolean isSetter(Method m) {
        return m.getName().startsWith("set") && m.getParameterCount() == 1;
    }

    private static boolean isGetter(Method m) {
        if (m.getParameterCount() != 0 || m.getReturnType().equals(Void.TYPE)) { /// getter has 0 arguments and returns something
            return false;
        }

        if (m.getName().startsWith("get")) {
            return true;
        } else if (m.getReturnType()
                    .isAssignableFrom(Boolean.class)) {/// getter on bools - can start with is,get or has
            return m.getName().startsWith("is") || m.getName().startsWith("has");
        } else {
            return false;
        }
    }

    public Method getMethod() {
        return method;
    }

    public String getPropertyName() {
        return propertyName;
    }

    public Class<?> getPropertyType() {
        return propertyType;
    }

    private static class AnnotatedSetter extends AnnotatedAccessor {

        public AnnotatedSetter(Method method) {
            super(method);
        }

        @Override
        protected Class<?> extractPropertyTypeNameFromMethod(Method m) {
            return m.getParameterTypes()[0];
        }

        @Override
        protected String extractPropertyNameFromMethod(Method m) {
            String trimmedName = m.getName().substring(3); /// always starts with set
            return Introspector.decapitalize(trimmedName);
        }
    }

    private static class AnnotatedGetter extends AnnotatedAccessor {
        public AnnotatedGetter(Method method) {
            super(method);
        }

        @Override
        protected String extractPropertyNameFromMethod(Method m) {
            String trimmedName;

            if (m.getName().startsWith("is")) {
                trimmedName = m.getName().substring(2);
            } else if (m.getName().startsWith("get") || m.getName().startsWith("has")) {
                trimmedName = m.getName().substring(3);
            } else {
                throw new MetamodelInitializationException("Failed to extract property name from annotated getter.");
            }
            return Introspector.decapitalize(trimmedName);
        }

        @Override
        protected Class<?> extractPropertyTypeNameFromMethod(Method m) {
            return m.getReturnType();
        }
    }

}
