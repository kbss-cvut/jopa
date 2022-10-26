package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the conversion of a Basic field or property.
 * <p>
 * The {@code Convert} annotation should not be used to specify conversion of the following: Id attributes, version
 * attributes, relationship attributes, and attributes explicitly denoted as Enumerated or Temporal. Applications that
 * specify such conversions will not be portable.
 * <p>
 * The {@code Convert} annotation may be applied to a basic attribute or to an element collection of basic type (in
 * which case the converter is applied to the elements of the collection).
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Convert {

    /**
     * Specifies the converter to be applied.
     * <p>
     * A value for this element must be specified if multiple converters would otherwise apply.
     */
    Class<?> converter() default void.class;

    /**
     * Used to disable an auto-apply converter. If disableConversion is true, the converter element should not be
     * specified.
     */
    boolean disableConversion() default false;
}
