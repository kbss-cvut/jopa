package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.TupleElement;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

import java.util.List;

public abstract class SelectionImpl<X> implements Selection<X>, TupleElement<X> {
    protected final Class<X> type;

    public SelectionImpl(Class<X> type) {
        this.type = type;
    }

    @Override
    public boolean isCompoundedSelection() {
        return false;
    }

    @Override
    public List<Selection<?>> getCompoundedSelectionItems() {
        throw new IllegalStateException();
    }

    @Override
    public Class<? extends X> getJavaType() {
        return this.type;
    }

    @Override
    public String getAlias() {
        return null;
    }
}
