package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Specifies that the field values are inferred.
 * <p/>
 * Inferred values are read only and cannot be modified.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Inferred {

    /**
     * Returns {@code true} when explicit data can also be included in the values.
     *
     * @return Whether to include explicit (asserted) data
     */
    boolean includeExplicit() default true;
}
