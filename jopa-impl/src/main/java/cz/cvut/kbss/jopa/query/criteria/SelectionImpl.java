/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Selection;

import java.util.List;

public abstract class SelectionImpl<X> implements Selection<X> {
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
