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
package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.query.sparql.SparqlConstants;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

class SoqlAttribute extends SoqlParameter {

    private static final String TRIPLE_END = " . ";

    private String value;

    private boolean isNot;

    private FilterableExpression operator;

    private boolean isOrderBy;

    private boolean isGroupBy;

    private boolean projected;

    public SoqlAttribute(SoqlNode firstNode) {
        super(firstNode);
        firstNode.setSoqlAttribute(this);
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public boolean isNot() {
        return isNot;
    }

    public void setNot(boolean not) {
        isNot = not;
    }

    public void setOperator(FilterableExpression operator) {
        this.operator = operator;
    }

    public boolean isOrderBy() {
        return isOrderBy;
    }

    public void setOrderBy(boolean orderBy) {
        isOrderBy = orderBy;
    }

    public boolean isGroupBy() {
        return isGroupBy;
    }

    public void setGroupBy(boolean groupBy) {
        isGroupBy = groupBy;
    }

    @Override
    public void setFirstNode(SoqlNode firstNode) {
        super.setFirstNode(firstNode);
        firstNode.setSoqlAttribute(this);
    }

    public boolean isProjected() {
        return projected;
    }

    public void setProjected(boolean projected) {
        this.projected = projected;
    }

    public boolean requiresFilter() {
        return (operator != null && operator.requiresFilterExpression()) || getFirstNode().requiresFilterExpression();
    }

    public boolean isObject() {
        return !getFirstNode().hasChild();
    }

    public boolean isInstanceOf() {
        return !getFirstNode().hasChild() && operator == null;
    }

    public List<String> getFilterExpressions(String rootVariable) {
        assert requiresFilter();
        String filterParam = getAsParam(rootVariable);
        final String filterValue = value != null ? SoqlUtils.soqlVariableToSparqlVariable(value) : null;
        if (getFirstNode().requiresFilterExpression()) {
            filterParam = getFirstNode().toFilterExpression(filterParam, filterValue);
        }
        if (operator != null && operator.requiresFilterExpression()) {
            return List.of(operator.toFilterExpression(filterParam, filterValue));
        } else {
            return List.of(filterValue != null ? filterParam + " = " + filterValue : filterParam);
        }
    }

    public List<String> getBasicGraphPattern(String rootVariable, Map<FieldSpecification<?, ?>, TriplePatternEnhancer> tpEnhancers) {
        if (isInstanceOf()) {
            return List.of(rootVariable + " " + SoqlConstants.RDF_TYPE + " " + toIri(getFirstNode()) + TRIPLE_END);
        } else {
            if (isObject()) {
                return List.of();
            }
            return buildTriplePatterns(rootVariable, tpEnhancers);
        }
    }

    private List<String> buildTriplePatterns(String rootVariable, Map<FieldSpecification<?, ?>, TriplePatternEnhancer> tpEnhancers) {
        String buildParam = "?" + getFirstNode().getValue();
        SoqlNode pointer = getFirstNode();

        return triplePatterns(pointer, 0, rootVariable, buildParam, tpEnhancers);
    }

    private List<String> triplePatterns(SoqlNode pointer, int depth, String rootVariable, String buildParam, Map<FieldSpecification<?, ?>, TriplePatternEnhancer> tpEnhancers) {
        final List<String> triplePatterns = new ArrayList<>();
        for (SoqlNode newPointer: pointer.getChildren()) {
            if (newPointer.getIri().isEmpty()) {
                triplePatterns.addAll(triplePatterns(newPointer, depth, rootVariable, buildParam, tpEnhancers));
                continue;
            }
            final String variable = depth == 0 ? rootVariable : "?" + pointer.getValue();
            buildParam += newPointer.getCapitalizedValue();
            final String param = buildTriplePatternObject(newPointer, buildParam);
            final TriplePatternEnhancer triplePatternEnhancer = tpEnhancers.computeIfAbsent(newPointer.getAttribute(), TriplePatternEnhancer::create);
            triplePatterns.addAll(triplePatternEnhancer.getTriplePatterns(variable, toIri(newPointer), param));
            triplePatterns.addAll(triplePatterns(newPointer, depth + 1, rootVariable, buildParam, tpEnhancers));
        }
        return triplePatterns;
    }

    private String buildTriplePatternObject(SoqlNode newPointer, String buildParam) {
        final String param;
        if ((newPointer.hasChild() && !newPointer.getChild().isIdentifier()) || value == null && !newPointer.occursInFilter()) {
            param = "?" + newPointer.getValue();
        } else {
            if (requiresFilter() || newPointer.occursInFilter()) {
                param = buildParam;
            } else {
                param = SoqlUtils.soqlVariableToSparqlVariable(value);
            }
        }
        return param;
    }

    private static String toIri(SoqlNode node) {
        final String nodeIri = node.getIri();
        return SparqlConstants.RDF_TYPE_SHORTCUT.equals(nodeIri) ? nodeIri : IdentifierTransformer.stringifyIri(nodeIri);
    }

    @Override
    public String toString() {
        return (value != null ? value + "." : "") + (getFirstNode() != null ? getFirstNode().toString() : " ");
    }
}
