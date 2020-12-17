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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;

import java.util.List;

public class CriteriaQueryImpl<X> extends QueryImpl implements CriteriaQuery<X> {
    public CriteriaQueryImpl(final QueryHolder query, final ConnectionWrapper connection) {
        super(query, connection);
    }

    @Override
    public CriteriaQuery<X> selectAll() {
        return null;
    }

    @Override
    public CriteriaQuery<X> select(Selection<? extends X> selection) {
        return null;
    }

    @Override
    public CriteriaQuery<X> where(Expression<Boolean> expression) {
        return null;
    }

    @Override
    public CriteriaQuery<X> where(Predicate... predicates) {
        return null;
    }

    @Override
    public List<X> getResultList() {
        return null;
    }

    @Override
    public X getSingleResult() {
        return null;
    }

    @Override
    public CriteriaQuery<X> setMaxResults(int i) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setFirstResult(int i) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setParameter(int i, Object o) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setParameter(int i, String s, String s1) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setParameter(String s, Object o) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setParameter(String s, String s1, String s2) {
        return null;
    }

    @Override
    public <T> CriteriaQuery<X> setParameter(Parameter<T> parameter, T t) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setParameter(Parameter<String> parameter, String s, String s1) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setUntypedParameter(int i, Object o) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setUntypedParameter(String s, Object o) {
        return null;
    }

    @Override
    public <T> CriteriaQuery<X> setUntypedParameter(Parameter<T> parameter, T t) {
        return null;
    }

    @Override
    public CriteriaQuery<X> setDescriptor(Descriptor descriptor) {
        return null;
    }
}
