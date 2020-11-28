package cz.cvut.kbss.jopa.model.metamodel;

/**
 * Instances of the type ListAttribute represent persistent
 * java.util.List-valued attributes defined by a query.
 *
 * @param <X>
 *            The type the represented List belongs to
 * @param <E>
 *            The element type of the represented List
 */
public interface ListQueryAttribute<X, E> extends PluralAttribute<X, java.util.List<E>, E> {

    //TODO determine which methods to add from ListAttribute
}
