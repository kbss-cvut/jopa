package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used to define one or more {@link SparqlResultSetMapping} annotations.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface SparqlResultSetMappings {

    /**
     * One or more {@code SparqlResultSetMapping} annotations.
     */
    SparqlResultSetMapping[] value();
}
