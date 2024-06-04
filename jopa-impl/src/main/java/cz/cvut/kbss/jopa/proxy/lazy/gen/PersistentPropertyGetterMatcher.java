package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.model.metamodel.AnnotatedAccessor;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistentPropertyMatcher;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.description.type.TypeDescription;

import java.beans.Introspector;
import java.util.Optional;

/**
 * Matches only persistent field getters.
 * <p>
 * This matcher checks that there exists a persistent field corresponding to the method name that appears to be a
 * getter. {@code get, is, has} prefixes are supported.
 *
 * @param <T> MethodDescription
 */
public class PersistentPropertyGetterMatcher<T extends MethodDescription> extends PersistentPropertyMatcher<T> {

    public PersistentPropertyGetterMatcher(Class<?> parentType) {
        super(parentType);
    }

    @Override
    protected boolean doMatch(T target) {
        final String name = target.getName();
        final Optional<String> fieldName = resolveFieldName(name, target);
        return fieldName.flatMap(this::tryFindingField)
                        // Field must not be transient, and it must not be an identifier (no need generating getter interceptor
                        // for identifier)
                        .map(f -> !EntityPropertiesUtils.isFieldTransient(f))
                        .orElse(false);
    }

    @Override
    protected Optional<String> resolveFieldName(String methodName, MethodDescription methodDesc) {
        assert methodDesc.getParameters().isEmpty();
        final TypeDescription.Generic returnType = methodDesc.getReturnType();
        if (methodName.startsWith(AnnotatedAccessor.GET_PREFIX)) {
            return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.GET_PREFIX.length())));
        } else if (methodName.startsWith(AnnotatedAccessor.IS_PREFIX) && (returnType.represents(Boolean.class))) {
            return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.IS_PREFIX.length())));
        } else if (methodName.startsWith(AnnotatedAccessor.HAS_PREFIX) && methodDesc.getReturnType()
                                                                                    .represents(Boolean.class)) {
            return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.HAS_PREFIX.length())));
        } else {
            return Optional.empty();
        }
    }
}

