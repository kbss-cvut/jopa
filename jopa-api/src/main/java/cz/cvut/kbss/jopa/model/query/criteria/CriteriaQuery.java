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
package cz.cvut.kbss.jopa.model.query.criteria;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;

import java.util.List;
import java.util.stream.Stream;

/**
 * Interface used to control the execution of typed queries.
 *
 * @param <X> Query result type
 */
public interface CriteriaQuery<X> extends Query {

    //TODO Javadoc for selectAll method
    CriteriaQuery<X> selectAll();

    //TODO Javadoc for selectAll method
    CriteriaQuery<X> select(Selection<? extends X> selection);

    /**
     * {@inheritDoc}
     */
    @Override
    List<X> getResultList();

    /**
     * {@inheritDoc}
     */
    @Override
    default Stream<X> getResultStream() {
        return getResultList().stream();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    X getSingleResult();

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setMaxResults(int maxResult);

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setFirstResult(int startPosition);

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setParameter(int position, Object value);

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setParameter(int position, String value, String language);

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setParameter(String name, Object value);

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setParameter(String name, String value, String language);

    /**
     * {@inheritDoc}
     */
    @Override
    <T> CriteriaQuery<X> setParameter(Parameter<T> parameter, T value);

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setParameter(Parameter<String> parameter, String value, String language);

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setUntypedParameter(int position, Object value);

    /**
     * {@inheritDoc}
     */
    @Override
    CriteriaQuery<X> setUntypedParameter(String name, Object value);

    /**
     * {@inheritDoc}
     */
    @Override
    <T> CriteriaQuery<X> setUntypedParameter(Parameter<T> parameter, T value);

    /**
     * Sets descriptor to use with this query.
     * <p>
     * The descriptor may specify contexts and languages for the retrieved query results. Note that the descriptor
     * applies only to results of managed types, i.e. when the result type of the query is a managed type. Otherwise,
     * the descriptor is ignored.
     * <p>
     * Use of descriptor may lead to additional result filtering, e.g. when the individual, which is a result of the
     * query, does not match criteria in the descriptor (it is in a different context, for instance), it is not returned
     * by {@link #getResultList()} and {@link #getSingleResult()}.
     *
     * @param descriptor The descriptor to use
     * @return This query instance
     */
    CriteriaQuery<X> setDescriptor(Descriptor descriptor);
}