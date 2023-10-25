/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.TypedQueryImpl;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionLiteralImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.ParameterExpressionImpl;
import cz.cvut.kbss.jopa.query.soql.SoqlConstants;

import java.util.HashMap;
import java.util.Map;

public class CriteriaParameterFiller {
    private final HashMap<String, ExpressionLiteralImpl> literalParameters;
    private int counter;

    public CriteriaParameterFiller() {
        this.literalParameters = new HashMap<>();
        this.counter = 0;
    }

    /**
     * Register literal expression as query parameter and return generated name for query.
     *
     * @param parameter - literal expression
     * @return String - generated name for query
     */
    public String registerParameter(ExpressionLiteralImpl parameter) {
        String name = generateParameterName();
        literalParameters.put(name, parameter);
        return SoqlConstants.VARIABLE_PREFIX + name;
    }

    /**
     * Register parameter expression as query parameter. Return real name if exists, generated name otherwise.
     *
     * @param parameter - parameter expression
     * @return String - real name for query if exists, generated name for query otherwise
     */
    public String registerParameter(ParameterExpression parameter) {
        if (parameter.getName() == null) {
            String name = generateParameterName();
            ((ParameterExpressionImpl) parameter).setNameIfUnnamed(name);
        }
        return SoqlConstants.VARIABLE_PREFIX + parameter.getName();
    }

    /**
     * Sets value from literal expressions registered as parameters to query parameters.
     *
     * @param query - TypedQuery fom setting parameters value
     */
    public <T> void setValuesToRegisteredParameters(TypedQueryImpl<T> query) {
        for (Map.Entry<String, ExpressionLiteralImpl> e : literalParameters.entrySet()) {
            if (e.getValue().getLanguageTag() != null) {
                query.setParameter(e.getKey(), (String) e.getValue().getValue(), e.getValue().getLanguageTag());
            } else {
                query.setParameter(e.getKey(), literalParameters.get(e.getKey()).getValue());
            }
        }
    }

    private String generateParameterName() {
        return "generatedName" + this.counter++;
    }
}
