/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.query.criteria.PathImpl;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public abstract class AbstractPathExpression<X> extends AbstractExpression<X> implements Path<X> {

    protected AbstractPathExpression pathSource;
    protected final Metamodel metamodel;

    public AbstractPathExpression(Class<X> type, AbstractPathExpression pathSource, Metamodel metamodel, CriteriaBuilder cb) {
        super(type, cb);
        this.pathSource = pathSource;
        this.metamodel = metamodel;
    }

    public <Y> Path<Y> getAttr(String attributeName) {
        Attribute attribute = metamodel.entity(type).getAttribute(attributeName);
        return new PathImpl<>(this.metamodel, this, attribute, this.cb);
    }

    public <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute) {
        return new PathImpl<>(this.metamodel, this, attribute, this.cb);
    }


    public Path<?> getParentPath() {
        return this.pathSource;
    }

}


