package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Specifies that a persistent property or field should be persisted as an enumerated type.
 * <p>
 * If the enumerated type is not specified or the Enumerated annotation is not used, the EnumType value is assumed to be
 * {@code STRING}.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Enumerated {

    /**
     * (Optional) The type used in mapping an enum type.
     *
     * @return Enum mapping type
     */
    EnumType value() default EnumType.STRING;
}
