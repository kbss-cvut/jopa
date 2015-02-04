package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Specifies that the annotated field contains only explicit (asserted) values.
 * <p/>
 * Explicit fields can be modified.
 * <p/>
 * Created by ledvima1 on 21.11.14.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Asserted {
}
