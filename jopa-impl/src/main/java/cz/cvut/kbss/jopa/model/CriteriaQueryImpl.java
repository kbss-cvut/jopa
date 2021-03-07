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

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.*;
import cz.cvut.kbss.jopa.query.criteria.CriteriaQueryHolder;
import cz.cvut.kbss.jopa.query.criteria.RootImpl;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import java.util.Objects;

//TODO PRO - CriteriaQueryImpl methods implementation
public class CriteriaQueryImpl<T> implements CriteriaQuery<T> {

    private static final Logger LOG = LoggerFactory.getLogger(CriteriaQuery.class);

    private final CriteriaQueryHolder<T> query;
    private final Metamodel metamodel;


    public CriteriaQueryImpl(CriteriaQueryHolder<T> query, Metamodel metamodel) {
        this.query = Objects.requireNonNull(query, ErrorUtils.getNPXMessageSupplier("query"));
        this.metamodel = metamodel;
    }

    @Override
    public <X> Root<X> from(Class<X> entityClass) {
        RootImpl<X> root = new RootImpl<>(metamodel, entityClass);
        query.setRoot(root);
        return null;
    }

    @Override
    public <X> Root<X> from(EntityType<X> entity) {
        return null;
    }

    @Override
    public CriteriaQuery<T> select(Selection<? extends T> selection){
        query.setSelection(selection);
        return this;
    }

    protected String getSoqlQueryRepresentation() {
        query.getSelection();
        query.isDistinct();
        query.getWhere();
        query.getOrderBy();
        return null;
    }

    @Override
    public CriteriaQuery<T> where(Expression<Boolean> expression) {
        query.setWhere(expression);
        return this;
    }

    @Override
    public CriteriaQuery<T> where(Predicate... predicates) {

        return this;
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
        return (Selection<T>) query.getSelection();
    }

    @Override
    public Predicate getRestriction() {
        return null;
    }
}