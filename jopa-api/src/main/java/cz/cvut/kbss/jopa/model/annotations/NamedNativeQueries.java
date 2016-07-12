package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies multiple native SPARQL named queries.
 * <p>
 * Query names are scoped to the persistence unit. The {@code NamedNativeQueries} annotation can be applied to an entity or mapped superclass.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface NamedNativeQueries {

    /**
     * (Required) Array of {@code NamedNativeQuery} annotations.
     */
    NamedNativeQuery[] value();
}
