package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Represents an OWL individual (or an RDF resource) identified by the specified value.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Individual {

    /**
     * Identifier of this individual.
     *
     * @return IRI as string
     */
    String iri();
}
