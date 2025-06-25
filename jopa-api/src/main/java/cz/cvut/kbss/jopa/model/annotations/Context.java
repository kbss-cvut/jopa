package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Declares repository context in which data are stored.
 * <p>
 * When used on a type, it is applied to all attributes of the type. When used on an attribute, it is applied to the
 * attribute values (the attribute assertions themselves are still stored in the subject's context).
 */
@Inherited
@Documented
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Context {

    /**
     * Context IRI.
     * <p>
     * Can use prefixed values as long as the prefix is declared.
     *
     * @return Context identifier
     */
    String value();

    /**
     * Whether to propagate the context recursively.
     * <p>
     * When applied on a type, this means propagate the context to all referenced values and keep propagating it
     * recursively.
     * <p>
     * If false (default), the context is applied only to the subject type (or attribute values). Further references are
     * reset to the default context.
     *
     * @return Whether to propagate the context
     */
    boolean propagate() default false;
}
