package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies a named native SPARQL query.
 * <p>
 * Query names are scoped to the persistence unit. The NamedNativeQuery annotation can be applied to an entity or mapped superclass.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface NamedNativeQuery {

    /**
     * The name used to refer to the query with the {@link cz.cvut.kbss.jopa.model.EntityManager} methods that create query objects.
     */
    String name();

    /**
     * The SPARQL query string.
     */
    String query();
}
