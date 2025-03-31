/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.metamodel.Bindable;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;

public class PathImpl<X> extends AbstractPathExpression<X> implements Path<X> {

    protected final String attributeName;
    protected final FieldSpecification<?, ?> attribute;

    public PathImpl(Metamodel metamodel, AbstractPathExpression pathSource, FieldSpecification<?, ?> attribute,
                    CriteriaBuilder cb) {
        super(resolveBindableJavaType(attribute), pathSource, metamodel, cb);
        this.attribute = attribute;
        this.attributeName = attribute.getName();
    }

    private static <E> Class<E> resolveBindableJavaType(FieldSpecification<?, ?> attribute) {
        if (attribute instanceof TypesSpecification) {
            return ((TypesSpecification) attribute).getElementType();
        }
        assert attribute instanceof Bindable;
        return ((Bindable<E>) attribute).getBindableJavaType();
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        if (this.pathSource != null) {
            this.pathSource.setExpressionToQuery(query, parameterFiller);
            query.append('.').append(attributeName);
        } else {
            query.append(attributeName);
        }
    }

}
