/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom.query;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.metamodel.AbstractQueryAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SingularQueryAttributeStrategy<X> extends QueryFieldStrategy<AbstractQueryAttribute<? super X, ?>, X> {

    private static final Logger LOG = LoggerFactory.getLogger(SingularQueryAttributeStrategy.class);

    private Object value;

    public SingularQueryAttributeStrategy(EntityType<X> et, AbstractQueryAttribute<? super X, ?> attribute) {
        super(et, attribute);
    }

    @Override
    public void addValueFromTypedQuery(TypedQuery<?> typedQuery) {
        Object querySingleResult;
        try {
            querySingleResult = typedQuery.getSingleResult();
        } catch (NoResultException e1) {
            LOG.trace("Query \"{}\" for attribute {} did not return any result.", attribute.getQuery(), attribute);
            return;
        } catch (NoUniqueResultException e2) {
            LOG.warn("Query \"{}\" for attribute {} does not have a unique result.", attribute.getQuery(), attribute);
            return;
        }

        if (!isValidRange(querySingleResult)) {
            return;
        }

        this.value = toAttributeValue(querySingleResult);
    }

    @Override
    public void buildInstanceFieldValue(Object instance) {
        setValueOnInstance(instance, value);
    }
}
