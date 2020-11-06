package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation can be used together with {@link cz.cvut.kbss.jopa.model.annotations.Sparql}
 * to add parameters to the SPARQL query.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Bind {

    /**
     * Name of the parameter used in the SPARQL query.
     */
    String sparqlParamName();
}
