package cz.cvut.kbss.jopa.model.metamodel;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;

/**
 * A simple wrapper that enables using annotations from methods while processing fields during metamodel creation.
 * @see AnnotatedAccessor
 */
public interface PropertyInfo {
    <T extends Annotation> T getAnnotation(Class<T> annotationClass);

    Class<?> getType();

    String getName();

    Member getMember();

    Field getField();

    static PropertyInfo from(Field field) {
        return new FieldInfo(field);
    }

    static PropertyInfo from(Method method,Field field) {
        return new MethodInfo(method,field);
    }

    class FieldInfo implements PropertyInfo {
        private final Field field;

        public FieldInfo(Field field) {
            this.field = field;
        }

        @Override
        public Class<?> getType() {
            return field.getType();
        }

        @Override
        public String getName() {
            return field.getName();
        }

        @Override
        public Member getMember() {
            return field;
        }

        @Override
        public <T extends Annotation> T getAnnotation(Class<T> annotationClass) {
            return field.getAnnotation(annotationClass);
        }

        @Override
        public Field getField() {
            return field;
        }

        @Override
        public String toString() {
            return "FieldInfo{" +
                    "class =" + field.getDeclaringClass().getName() +
                    ", field=" + field.getName() +
                    '}';
        }
    }

    class MethodInfo implements PropertyInfo {
        private final Method method;

        private final Field rawField;

        public MethodInfo(Method method, Field rawField) {
            this.method = method;
            this.rawField = rawField;
        }

        @Override
        public Class<?> getType() {
            return rawField.getType();
        }

        @Override
        public String getName() {
            return rawField.getName();
        }
        @Override
        public Member getMember() {
            return method;
        }

        @Override
        public <T extends Annotation> T getAnnotation(Class<T> annotationClass) {
            return method.getAnnotation(annotationClass);
        }

        @Override
        public Field getField() {
            return rawField;
        }

        @Override
        public String toString() {
            return "MethodInfo{" +
                    "method=" + method +
                    ", rawField=" + rawField +
                    '}';
        }
    }
}

