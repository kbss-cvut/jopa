package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.List;

//TODO PRO - selection interface - methods
public interface Selection<X> {
    boolean isCompoundedSelection();
    List<Selection<?>> getCompoundedSelectionItems();
    Selection<X> alias(String name);
}
