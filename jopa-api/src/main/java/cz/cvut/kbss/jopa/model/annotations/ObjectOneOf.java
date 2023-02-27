package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that instances of the annotated type represent a fixed enumeration of objects.
 * <p>
 * This annotation should be used in conjunction with {@link Individual} when mapping enum constants to ontological
 * individuals representing the OWL {@literal ObjectOneOf} concept description.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface ObjectOneOf {
}
