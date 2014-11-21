package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Specifies that the field values are inferred.
 * <p/>
 * Inferred values are read only and cannot be modified.
 * <p/>
 * Created by ledvima1 on 21.11.14.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Inferred {

    /**
     * Returns {@code true} when explicit data can also be included in the values.
     *
     * @return
     */
    public boolean includeExplicit() default true;
}
