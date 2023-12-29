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

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class ExpressionLiteralImpl<T> extends AbstractExpression<T>  {

    private final Object literal;
    private final String languageTag;

    public ExpressionLiteralImpl(T literal, CriteriaBuilder cb) {
        super(determineClass(literal), cb);
        this.literal = literal;
        languageTag = null;
    }

    public ExpressionLiteralImpl(String literal, String languageTag, CriteriaBuilder cb) {
        super(determineClass(literal), cb);
        this.literal = literal;
        this.languageTag = languageTag;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(parameterFiller.registerParameter(this));
    }

    private static Class determineClass(Object literal) {
        return literal.getClass();
    }

    public Object getValue(){
        return literal;
    }

    public String getLanguageTag(){
        return languageTag;
    }

}
