package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

import java.util.List;

public class SelectionImpl<T> implements Selection<T> {

    protected final Attribute<T, ?> attribute;
    //    protected final Class<T> selectAll;

    public SelectionImpl(Attribute<T, ?> attribute) {
        this.attribute = attribute;
    }

//    public <X> SelectionImpl(Attribute<T, X> attribute) {
//        this.attribute = attribute;
//        this.selectAll = null;
//    }
//
//    public <X> SelectionImpl(Class<T> selectAll) {
//        this.attribute = null;
//        this.selectAll = selectAll;
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
    public Selection<T> alias(String name) {
        return null;
    }
}
