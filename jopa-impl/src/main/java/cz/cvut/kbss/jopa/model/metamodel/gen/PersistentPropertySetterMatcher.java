package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.model.metamodel.AnnotatedAccessor;
import net.bytebuddy.description.method.MethodDescription;

import java.beans.Introspector;
import java.util.Optional;

/**
 * Matches only persistent attribute setters.
 * <p>
 * This matcher checks that there exists a persistent field corresponding to the method name that appears to be a
 * setter.
 *
 * @param <T> MethodDescription
 */
public class PersistentPropertySetterMatcher<T extends MethodDescription> extends PersistentPropertyMatcher<T> {

    public PersistentPropertySetterMatcher(Class<?> parentType) {
        super(parentType);
    }

    @Override
    protected Optional<String> resolveFieldName(String methodName, MethodDescription methodDesc) {
        assert methodName.startsWith(AnnotatedAccessor.SET_PREFIX) && methodDesc.getParameters().size() == 1;

        return Optional.of(Introspector.decapitalize(methodName.substring(AnnotatedAccessor.SET_PREFIX.length())));
    }
}
