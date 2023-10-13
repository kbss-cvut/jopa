package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.model.metamodel.AnnotatedAccessor;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.matcher.ElementMatcher;

import java.beans.Introspector;
import java.lang.reflect.Field;
import java.util.Optional;

/**
 * Matches only getters/setters.
 * <p>
 * This matcher checks that there exists a persistent field corresponding to the method name that appears to be a
 * getter/setter.
 *
 * @param <T> MethodDescription
 */
class PersistentPropertyMatcher<T extends MethodDescription> extends ElementMatcher.Junction.ForNonNullValues<T> {

    private final Class<?> parentType;

    public PersistentPropertyMatcher(Class<?> parentType) {this.parentType = parentType;}

    @Override
    protected boolean doMatch(T target) {
        final String name = target.getName();
        final Optional<String> fieldName = resolveFieldName(name, target);
        return fieldName.flatMap(this::tryFindingField).map(f -> !EntityPropertiesUtils.isFieldTransient(f))
                        .orElse(false);
    }

    private Optional<String> resolveFieldName(String methodName, MethodDescription methodDesc) {
        if (methodName.startsWith(AnnotatedAccessor.SET_PREFIX) && methodDesc.getParameters().size() == 1) {
            return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.SET_PREFIX.length())));
        } else if (methodDesc.getParameters().isEmpty()) {
            final TypeDescription.Generic returnType = methodDesc.getReturnType();
            if (methodName.startsWith(AnnotatedAccessor.GET_PREFIX)) {
                return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.GET_PREFIX.length())));
            } else if (methodName.startsWith(AnnotatedAccessor.IS_PREFIX) && (returnType.represents(Boolean.class))) {
                return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.IS_PREFIX.length())));
            } else if (methodName.startsWith(AnnotatedAccessor.HAS_PREFIX) && methodDesc.getReturnType()
                                                                                        .represents(Boolean.class)) {
                return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.HAS_PREFIX.length())));
            }
        }
        return Optional.empty();
    }

    private Optional<Field> tryFindingField(String fieldName) {
        Class<?> type = parentType;
        do {
            try {
                Field f = type.getDeclaredField(fieldName);
                return Optional.of(f);
            } catch (NoSuchFieldException e) {
                type = type.getSuperclass();
            }
        } while (type != null);
        return Optional.empty();
    }
}
