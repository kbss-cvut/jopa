package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;

/**
 * Instances of the type {@code RDFContainerAttribute} represent attributes mapped to RDF containers.
 * <p>
 * RDF containers may be represented by different collections, depending on the type of the container. While a
 * {@literal rdf:Seq} is likely to be represented by a {@link java.util.List}, as it allows duplicates but is ordered, a
 * {@literal rdf:Alt} represents a set of alternatives and will thus probably be represented by a {@link java.util.Set},
 * possibly an implementation preserving order. A {@literal rdf:Bag} allows duplicates and is unordered, but will likely
 * be represented also by a {@link java.util.List}.
 *
 * @param <X> The type the represented collection belongs to
 * @param <C> Type of the collection
 * @param <E> The element type of the represented collection
 */
@NonJPA
public interface RDFContainerAttribute<X, C, E> extends PluralAttribute<X, C, E> {

    /**
     * Type of the RDF container represented by this attribute.
     *
     * @return RDF container type
     */
    @NonJPA
    RDFContainerType getContainerType();
}
