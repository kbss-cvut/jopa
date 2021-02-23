/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;
import cz.cvut.kbss.jopa.query.criteria.CriteriaQueryHolder;
import cz.cvut.kbss.jopa.query.criteria.SelectionImpl;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.jopa.utils.Procedure;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import java.util.Objects;

//TODO PRO - CriteriaQueryImpl methods implementation
public class CriteriaQueryImpl<T> implements CriteriaQuery<T> {

    private static final Logger LOG = LoggerFactory.getLogger(CriteriaQuery.class);

    final CriteriaQueryHolder<T> query;


    public CriteriaQueryImpl(CriteriaQueryHolder<T> query) {
        this.query = Objects.requireNonNull(query, ErrorUtils.getNPXMessageSupplier("query"));
    }

    public String getSoqlQuery(){
        return query.assembleSoqlQuery();
    }

    @Override
    public CriteriaQuery<T> select(Selection<? extends T> selection) throws Exception {
        SelectionImpl sel = (SelectionImpl) selection;
        sel.getQuery();
        if (selection.isCompoundedSelection()){
            //TODO - compounded selection
            throw new Exception("Not implemented yet");
        } else {
            query.setSelection(selection);
        }
        return this;
    }

    @Override
    public CriteriaQuery<T> where(Expression<Boolean> expression) {
        return null;
    }

    @Override
    public CriteriaQuery<T> where(Predicate... predicates) {
        return null;
    }

    @Override
    public Class<T> getResultType() {
        return null;
    }

    @Override
    public CriteriaQuery<T> distinct(boolean b) {
        query.setDistinct(b);
        return this;
    }

    @Override
    public boolean isDistinct() {
        return query.isDistinct();
    }

    @Override
    public Selection<T> getSelection() {
        return query.getSelection();
    }

    @Override
    public Predicate getRestriction() {
        return null;
    }
}
