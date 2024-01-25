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
package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.query.criteria.PathImpl;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;

public abstract class AbstractPathExpression<X> extends AbstractExpression<X> implements Path<X> {

    protected AbstractPathExpression pathSource;
    protected final Metamodel metamodel;

    public AbstractPathExpression(Class<X> type, AbstractPathExpression pathSource, Metamodel metamodel,
                                  CriteriaBuilder cb) {
        super(type, cb);
        this.pathSource = pathSource;
        this.metamodel = metamodel;
    }

    public <Y> Path<Y> getAttr(String attributeName) {
        final EntityType<X> et = metamodel.entity(type);
        final FieldSpecification<? super X, ?> fs;
        if (et.getIdentifier().getName().equals(attributeName)) {
            fs = et.getIdentifier();
        } else if (et.getTypes() != null && et.getTypes().getName().equals(attributeName)) {
            fs = et.getTypes();
        } else {
            fs = et.getAttribute(attributeName);
        }
        return new PathImpl<>(this.metamodel, this, fs, this.cb);
    }

    public <Y> Path<Y> getAttr(SingularAttribute<? super X, Y> attribute) {
        return new PathImpl<>(this.metamodel, this, attribute, this.cb);
    }


    public Path<?> getParentPath() {
        return this.pathSource;
    }

}


