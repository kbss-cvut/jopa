package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.matcher.ElementMatcher;

import java.lang.reflect.Field;
import java.util.Optional;

/**
 * Matches only persistent attribute getters/setters.
 * <p>
 * This matcher checks that there exists a persistent field corresponding to the method name that appears to be a
 * getter/setter.
 *
 * @param <T> MethodDescription
 */
abstract class PersistentPropertyMatcher<T extends MethodDescription> extends ElementMatcher.Junction.ForNonNullValues<T> {

    private final Class<?> parentType;

    public PersistentPropertyMatcher(Class<?> parentType) {this.parentType = parentType;}

    @Override
    protected boolean doMatch(T target) {
        final String name = target.getName();
        final Optional<String> fieldName = resolveFieldName(name, target);
        return fieldName.flatMap(this::tryFindingField).map(f -> !EntityPropertiesUtils.isFieldTransient(f))
                        .orElse(false);
    }

    abstract Optional<String> resolveFieldName(String methodName, MethodDescription methodDesc);

    Optional<Field> tryFindingField(String fieldName) {
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
