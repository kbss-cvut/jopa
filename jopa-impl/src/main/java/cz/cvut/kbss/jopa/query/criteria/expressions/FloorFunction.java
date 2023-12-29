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

import cz.cvut.kbss.jopa.query.soql.SoqlConstants;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class FloorFunction<Y extends Number> extends AbstractFunctionExpression<Y> {

    public FloorFunction(Class<Y> type, AbstractPathExpression argumentExpression, CriteriaBuilder cb) {
        super(type, argumentExpression, cb);
    }

    @Override
    public String getFunctionName() {
        return SoqlConstants.Functions.FLOOR;
    }
}
