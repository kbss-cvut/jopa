package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Specifies basic mapping of an entity attribute to an ontological property assertion.
 * <p>
 * It is used on singular attributes, where it can be used to specify optionality of the field. This optionality in
 * essence corresponds to a {@link ParticipationConstraint} with min and max set to one.
 * <p>
 * If not specified, default values are used.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Basic {

    /**
     * (Optional) Defines whether the value of the field or property may be null.
     * <p>
     * If set to true, it corresponds to a {@link ParticipationConstraint} with min and max set to one.
     * <p>
     * <b>Default:</b> true
     */
    boolean optional() default true;

    /**
     * (Optional) Defines whether the value of the field should be lazily loaded or must be eagerly fetched. The EAGER
     * strategy is a requirement on the persistence provider runtime that the value must be eagerly fetched. The LAZY
     * strategy is a hint to the persistence provider runtime. If not specified, defaults to EAGER.
     * <p>
     * <b>Default:</b> {@link FetchType#EAGER}
     */
    FetchType fetch() default FetchType.EAGER;
}
