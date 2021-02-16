package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

import java.util.List;

public class SelectionImpl<T, X> implements Selection<T> {

//    protected final Class<T> type;
//    protected Attribute<T, X> attribute;
//
//    public SelectionImpl(Class<T> type, Attribute attribute) {
//        this.type = type;
//        this.attribute = attribute;
//    }

    @Override
    public boolean CompoundedSelection() {
        return false;
    }

    @Override
    public List<Selection<?>> getCompoundedSelectionItems() {
        return null;
    }

    @Override
    public Selection alias(String name) {
        return null;
    }
}
