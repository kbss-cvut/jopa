package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.List;

public interface Selection<X> {

    /**
     * Whether the selection item is a compound selection.
     * Compounded selection is NOT YET SUPPORTED. Therefore, the method always returns false.
     * @return boolean indicating whether the selection is a compound selection
     */
    boolean isCompoundedSelection();

    /**
     * Return the selection items composing a compound selection. Modifications to the list do not affect the query.
     * Compounded selection is NOT YET SUPPORTED. Therefore, the method always throws IllegalStateException.
     * @return list of selection items
     * @throws IllegalStateException if selection is not a compound selection
     */
    List<Selection<?>> getCompoundedSelectionItems() throws IllegalStateException;
}
