package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the inheritance strategy to be used for an entity class hierarchy.
 * <p>
 * It is specified on the entity class that is the root of the entity class hierarchy. If the {@link Inheritance}
 * annotation is not specified or if no inheritance type is specified for an entity class hierarchy, the TWO_STEP
 * loading strategy is used.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Inheritance {

    /**
     * The strategy to be used for the entity inheritance hierarchy.
     */
    InheritanceType strategy() default InheritanceType.TWO_STEP;
}
