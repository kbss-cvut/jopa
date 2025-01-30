package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies mapping of an <a href="https://www.w3.org/TR/rdf12-schema/#ch_containervocab">RDF container</a>.
 * <p>
 * RDF containers are resources used to represent collections. In contrast to RDF collections, they are not closed, so
 * it is possible to add new items to them without having to reattach the container ending element.
 * <p>
 * Three types of RDF containers are defined, each with different convention-based semantics.
 *
 * @see RDFContainerType
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface RDFContainer {

    /**
     * The type of the container.
     *
     * @return Type of the container
     */
    RDFContainerType type();
}
