package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.List;

public interface Selection<X> {

    /**
     * Whether the selection item is a compound selection.
     * @return boolean indicating whether the selection is a compound selection
     */
    boolean isCompoundedSelection();

    /**
     * Return the selection items composing a compound selection. Modifications to the list do not affect the query.
     * @return
     */
    List<Selection<?>> getCompoundedSelectionItems();

    /**
     * Assigns an alias to the selection item. Returns the same selection item.
     * @param name alias
     * @return selection item
     */
    Selection<X> alias(String name);
}
