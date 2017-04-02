package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the callback listener classes to be used for an entity or mapped superclass.
 * This annotation may be applied to an entity class or mapped superclass.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface EntityListeners {

    /**
     * The callback listener classes
     */
    Class<?>[] value();
}
