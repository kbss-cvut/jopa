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

import cz.cvut.kbss.jopa.exception.SoqlException;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.SparqlConstants;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

public class SoqlQueryListener extends SoqlBaseListener {

    private static final Logger LOG = LoggerFactory.getLogger(SoqlQueryListener.class);

    private final MetamodelImpl metamodel;

    private String soql;
    private String sparql;

    private String typeDef = SoqlConstants.SELECT;

    // keeps pointer at created object of SoqlAttribute while processing other necessary rules
    private SoqlAttribute attrPointer;

    private final List<SoqlAttribute> attributes;

    // keeps index of first object of SoqlAttribute after OR operator
    private final Set<SoqlAttribute> objectOfNextOr;

    private final List<SoqlOrderParameter> orderAttributes;

    private final List<SoqlGroupParameter> groupAttributes;

    private final Map<String, String> objectTypes;

    private final Map<FieldSpecification<?, ?>, TriplePatternEnhancer> tpEnhancers = new HashMap<>();

    private boolean isSelectedParamDistinct = false;

    private boolean isSelectedParamCount = false;

    private boolean isInObjectIdentifierExpression = false;

    private String projectedVariable;

    private String rootVariable = "?x";


    public SoqlQueryListener(MetamodelImpl metamodel) {
        this.metamodel = Objects.requireNonNull(metamodel);
        this.attributes = new ArrayList<>();
        this.objectOfNextOr = new HashSet<>();
        this.orderAttributes = new ArrayList<>();
        this.groupAttributes = new ArrayList<>();
        this.objectTypes = new HashMap<>();
    }

    @Override
    public void exitQuerySentence(SoqlParser.QuerySentenceContext ctx) {
        this.soql = ctx.getText();
        buildSparqlQueryString();
    }

    @Override
    public void enterSimpleSubpath(SoqlParser.SimpleSubpathContext ctx) {
        if (ctx.simpleSubpath() == null) {
            return;
        }

        if (ctx.getChildCount() == 1) {
            return;
        }

        // node was already processed by parent
        if (ctx.getParent() instanceof SoqlParser.SimpleSubpathContext) {
            return;
        }

        SoqlNode owner = linkSimpleSubpath(ctx);

        // don't add top level references multiple times
        if (!owner.hasChild() && objectTypes.containsKey(owner.getValue())) {
            return;
        }

        SoqlAttribute newAttr = new SoqlAttribute(owner);
        if (owner.hasChild() && isIdentifier(owner, owner.getChild())) {
            this.isInObjectIdentifierExpression = true;
            if (projectedVariable.equals(owner.getValue()) && currentPointerIsNotAttributeReference()) {
                attrPointer.setProjected(true);
            } else {
                newAttr.setProjected(true);
                pushNewAttribute(newAttr);
            }
        } else {
            pushNewAttribute(newAttr);
        }
    }

    private SoqlNode linkSimpleSubpath(ParserRuleContext ctx) {
        AttributeNode firstNode = new AttributeNode(getOwnerFromParam(ctx));
        AttributeNode currentNode = firstNode;

        while (ctx.getChildCount() == 3) {
            ctx = (ParserRuleContext) ctx.getChild(2);
            SoqlNode prevNode = currentNode;
            currentNode = new AttributeNode(prevNode, ctx.getChild(0).getText());
            prevNode.setChild(currentNode);
        }
        setIris(firstNode);
        if (currentNode.getIri().isEmpty()) {
            this.isInObjectIdentifierExpression = true;
            if (projectedVariable != null && projectedVariable.equals(firstNode.getValue()) && currentPointerIsNotAttributeReference()) {
                attrPointer.setProjected(true);
            }
        }
        return firstNode;
    }

    @Override
    public void enterSelectClause(SoqlParser.SelectClauseContext ctx) {
        this.typeDef = QueryType.SELECT.getKeyword();

        if (ctx.DISTINCT() != null) {
            this.isSelectedParamDistinct = true;
        }
    }

    private void pushNewAttribute(SoqlAttribute myAttr) {
        attributes.add(myAttr);
        this.attrPointer = myAttr;
    }

    private void popAttribute() {
        this.attrPointer = attributes.remove(attributes.size() - 1);
    }

    @Override
    public void exitSelectExpression(SoqlParser.SelectExpressionContext ctx) {
        if (!isSelectedParamCount) {
            this.projectedVariable = ctx.getText();
        }
    }

    @Override
    public void enterAggregateExpression(SoqlParser.AggregateExpressionContext ctx) {
        if (ctx.COUNT() != null) {
            isSelectedParamCount = true;
            if (ctx.DISTINCT() != null) {
                isSelectedParamDistinct = true;
            }

            if (ctx.simpleSubpath() != null) {
                this.projectedVariable = ctx.simpleSubpath().getText();
            }
        }
    }

    private boolean isIdentifier(SoqlNode objectNode, SoqlNode attributeNode) {
        if (!objectTypes.containsKey(objectNode.getValue())) {
            return false;
        }
        final String objectName = objectTypes.get(objectNode.getValue());
        IdentifiableEntityType<?> entityType = getEntityType(objectName);
        if (entityType == null) {
            return false;
        }
        return entityType.getIdentifier().getName().equals(attributeNode.getValue());
    }

    private boolean currentPointerIsNotAttributeReference() {
        return !attrPointer.getFirstNode().hasChild();
    }

    @Override
    public void exitConditionalTerm(SoqlParser.ConditionalTermContext ctx) {
        final ParserRuleContext parentCtx = ctx.getParent();
        if (parentCtx.getChildCount() > 1 && !parentCtx.getChild(0).equals(ctx)) {
            objectOfNextOr.add(attrPointer);
        }
    }

    @Override
    public void exitConditionalFactor(SoqlParser.ConditionalFactorContext ctx) {
        if (ctx.getChildCount() > 1) {
            attrPointer.setNot(true);
        }
    }

    @Override
    public void exitInExpression(SoqlParser.InExpressionContext ctx) {
        assert ctx.getChildCount() > 2;
        if (isInObjectIdentifierExpression) {
            pushNewAttribute(createSyntheticAttributeForEntityId());
        }
        final ParseTree value = resolveInExpressionValue(ctx);
        if (ctx.NOT() != null) {
            attrPointer.setOperator(InOperator.notIn());
        } else {
            attrPointer.setOperator(InOperator.in());
        }
        attrPointer.setValue(value.getText());
        this.isInObjectIdentifierExpression = false;
    }

    private SoqlAttribute createSyntheticAttributeForEntityId() {
        if (attrPointer.getFirstNode().hasChild()) {
            attrPointer.getFirstNode().getChild().setChild(null);
            return new SoqlAttribute(attrPointer.getFirstNode().getChild());
        }

        return new SoqlAttribute(new AttributeNode(rootVariable.substring(1)));
    }

    private ParseTree resolveInExpressionValue(SoqlParser.InExpressionContext ctx) {
        return ctx.inItem().get(ctx.inItem().size() - 1);
    }

    @Override
    public void exitLikeExpression(SoqlParser.LikeExpressionContext ctx) {
        if (ctx.getChildCount() > 2 && ctx.getChild(1).getText().equals(SoqlConstants.NOT)) {
            attrPointer.setOperator(LikeOperator.notLike());
            ParseTree whereClauseValue = ctx.getChild(3);
            attrPointer.setValue(whereClauseValue.getText());
        } else {
            attrPointer.setOperator(LikeOperator.like());
            ParseTree whereClauseValue = ctx.getChild(2);
            attrPointer.setValue(whereClauseValue.getText());
        }
        this.isInObjectIdentifierExpression = false;
    }

    @Override
    public void exitMemberOfExpression(SoqlParser.MemberOfExpressionContext ctx) {
        if (ctx.getChildCount() > 2 && ctx.getChild(1).getText().equals(SoqlConstants.NOT)) {
            attrPointer.setNot(true);
        }
        attrPointer.setOperator(MemberOfOperator.memberOf());
        ParseTree whereClauseValue = ctx.getChild(0);
        attrPointer.setValue(whereClauseValue.getText());
        this.isInObjectIdentifierExpression = false;
    }

    @Override
    public void exitComparisonExpression(SoqlParser.ComparisonExpressionContext ctx) {
        String operator = ctx.getChild(1).getText();

        ParseTree whereClauseValue = ctx.getChild(2);

        if (isInObjectIdentifierExpression) {
            assert Objects.equals(operator, "=");
            if (attrPointer.isProjected()) {
                this.rootVariable = SoqlUtils.soqlVariableToSparqlVariable(whereClauseValue.getText());
            }
            if (attributes.size() > 1) {
                if (isRootIdentifier(attrPointer)) {
                    // If the current attribute is identifier (i.e., something like root.iri = :iri) and there are other attributes to work with,
                    // just pop it and not use it in the query anymore
                    popAttribute();
                } else {
                    final String varName = whereClauseValue.getText();
                    attrPointer.getFirstNode().getChild().setValue(
                            varName.charAt(0) == SoqlConstants.VARIABLE_PREFIX ? varName.substring(1) : varName);
                }
            }
        } else {
            attrPointer.setOperator(new ComparisonOperator(operator));
            attrPointer.setValue(whereClauseValue.getText());
        }
        this.isInObjectIdentifierExpression = false;
    }

    private boolean isRootIdentifier(SoqlAttribute attribute) {
        return isIdentifier(attribute.getFirstNode(), attribute.getFirstNode().getChild()) &&
                !attribute.getFirstNode().getChild().hasChild();
    }

    @Override
    public void enterFromClause(SoqlParser.FromClauseContext ctx) {
        String entityName = ctx.entityName().getText();
        String identificationVariable = ctx.IDENTIFICATION_VARIABLE().getText();
        objectTypes.put(identificationVariable, entityName);
        SoqlNode node = new AttributeNode(entityName);
        setObjectIri(node);
        SoqlAttribute myAttr = new SoqlAttribute(node);
        pushNewAttribute(myAttr);
    }

    @Override
    public void exitFunctionsReturningStrings(SoqlParser.FunctionsReturningStringsContext ctx) {
        createFunctionNode(ctx);
    }

    private void createFunctionNode(RuleContext ctx) {
        final String functionName = ctx.getChild(0).getText();
        final FunctionNode node = new FunctionNode(attrPointer.getFirstNode(), functionName);
        attrPointer.setFirstNode(node);
    }

    @Override
    public void exitFunctionsReturningNumerics(SoqlParser.FunctionsReturningNumericsContext ctx) {
        createFunctionNode(ctx);
    }

    @Override
    public void exitFunctionsReturningBoolean(SoqlParser.FunctionsReturningBooleanContext ctx) {
        createFunctionNode(ctx);
    }

    @Override
    public void enterOrderByItem(SoqlParser.OrderByItemContext ctx) {
        SoqlNode firstNode = linkObjectPathExpression(ctx);
        String orderingBy = getOrderingBy(ctx);
        SoqlOrderParameter orderParam = new SoqlOrderParameter(firstNode, orderingBy);
        boolean attrSet = false;
        for (SoqlAttribute attr : attributes) {
            if (attr.getAsParam().equals(orderParam.getAsParam())) {
                orderParam.setAttribute(attr);
                attrSet = true;
            }
        }
        if (!attrSet) {
            SoqlAttribute myAttr = new SoqlAttribute(firstNode);
            myAttr.setValue(orderParam.getAsValue(rootVariable));
            myAttr.setOrderBy(true);
            attributes.add(1, myAttr);
            orderParam.setAttribute(myAttr);
        }
        orderAttributes.add(orderParam);
    }

    @Override
    public void enterGroupByItem(SoqlParser.GroupByItemContext ctx) {
        SoqlNode firstNode = linkObjectPathExpression(ctx);
        SoqlGroupParameter groupParam = new SoqlGroupParameter(firstNode);
        boolean attrSet = false;
        for (SoqlAttribute attr : attributes) {
            if (attr.getAsParam().equals(groupParam.getAsParam())) {
                groupParam.setAttribute(attr);
                attrSet = true;
            }
        }
        if (!attrSet) {
            SoqlAttribute myAttr = new SoqlAttribute(firstNode);
            myAttr.setValue(groupParam.getAsValue(rootVariable));
            myAttr.setGroupBy(true);
            attributes.add(1, myAttr);
            groupParam.setAttribute(myAttr);
        }
        groupAttributes.add(groupParam);
    }

    private SoqlNode linkObjectPathExpression(ParserRuleContext ctx) {
        SoqlNode firstNode = new AttributeNode(getOwnerFromParam(ctx));
        SoqlNode currentNode = firstNode;
        for (int i = 2; i < ctx.getChild(0).getChildCount(); i += 2) {
            SoqlNode prevNode = currentNode;
            currentNode = new AttributeNode(prevNode, ctx.getChild(0).getChild(i).getText());
            prevNode.setChild(currentNode);
        }
        setIris(firstNode);
        if (currentNode.getIri().isEmpty()) {
            currentNode.getParent().setChild(null);
            this.isInObjectIdentifierExpression = true;
        }
        return firstNode;
    }

    // Methods to help parse the tree
    private String getOwnerFromParam(ParserRuleContext ctx) {
        return ctx.getChild(0).getChild(0).getText();
    }

    private String getOrderingBy(ParserRuleContext ctx) {
        return ctx.getChildCount() > 1 ? ctx.getChild(1).getText() : "";
    }

    private void setObjectIri(SoqlNode node) {
        IdentifiableEntityType<?> entityType = getEntityType(node.getValue());
        if (entityType == null) {
            return;
        }
        node.setIri(entityType.getIRI().toString());
        if (node.hasChild()) {
            setAllNodesIris(entityType, node.getChild());
        }
    }

    private IdentifiableEntityType<?> getEntityType(String name) {
        for (EntityType<?> type : metamodel.getEntities()) {
            IdentifiableEntityType<?> entityType = (IdentifiableEntityType<?>) type;
            if (entityType.getName().equals(name)) {
                return entityType;
            }
        }
        return null;
    }

    private void setAllNodesIris(EntityType<?> entityType, SoqlNode node) {
        final String nodeName = node.getValue();
        if (entityType.getIdentifier().getName().equals(nodeName)) {
            return;
        }
        if (entityType.getTypes() != null && entityType.getTypes().getName().equals(node.getValue())) {
            node.setIri(SparqlConstants.RDF_TYPE_SHORTCUT);
            node.setAttribute(entityType.getTypes());
            return;
        }
        final Attribute<?, ?> att;
        try {
            att = entityType.getAttribute(node.getValue());
        } catch (IllegalArgumentException e) {
            throw new SoqlException("No matching attribute with name '" + node.getValue() + "' found on entity type '" + entityType.getName() + "'.");
        }
        //not implemented case of 3 or more fragments (chained SoqlNodes)
        node.setIri(att.getIRI().toString());
        node.setAttribute(att);
        if (node.hasChild()) {
            final Type<?> type = resolveBindableType(att);
            if (type.getPersistenceType() != Type.PersistenceType.ENTITY) {
                return;
            }
            setAllNodesIris((EntityType<?>) type, node.getChild());
        }
    }

    private static Type<?> resolveBindableType(Attribute<?, ?> att) {
        if (att.isCollection()) {
            return ((PluralAttribute<?, ?, ?>) att).getElementType();
        } else {
            return ((SingularAttribute<?, ?>) att).getType();
        }
    }

    private void setIris(SoqlNode firstNode) {
        if (!objectTypes.containsKey(firstNode.getValue())) {
            return;
        }
        String objectName = objectTypes.get(firstNode.getValue());
        IdentifiableEntityType<?> entityType = getEntityType(objectName);
        if (entityType == null) {
            return;
        }
        if (firstNode.hasChild()) {
            setAllNodesIris(entityType, firstNode.getChild());
        }
    }

    public String getSparqlQuery() {
        assert sparql != null;
        return sparql;
    }

    private String getSelectParameter(SoqlAttribute attribute) {
        SoqlNode firstNode = attribute.getFirstNode();
        if (!firstNode.hasChild()) {
            return rootVariable;
        }

        setIris(firstNode); // we have to reassign the iris, because the table was not initialized when the first attribute was set
        return attribute.getAsValue(rootVariable);
    }

    //Methods to build new Query
    private void buildSparqlQueryString() {
        if (attributes.isEmpty()) {
            return;
        }

        // the first attribute is either a projected parameter or a type attribute
        String selectParameter = getSelectParameter(attributes.get(0));

        StringBuilder newQueryBuilder = new StringBuilder(typeDef);
        if (isSelectedParamCount) {
            newQueryBuilder.append(getCountPart(selectParameter));
        } else {
            if (isSelectedParamDistinct) {
                newQueryBuilder.append(' ').append(SoqlConstants.DISTINCT);
            }
            newQueryBuilder.append(' ').append(selectParameter).append(' ');
        }
        newQueryBuilder.append("WHERE { ");
        newQueryBuilder.append(processSupremeAttributes());
        if (!objectOfNextOr.isEmpty()) {
            newQueryBuilder.append("{ ");
        }
        newQueryBuilder.append(processAttributes());
        if (!objectOfNextOr.isEmpty()) {
            newQueryBuilder.append("} ");
        }
        newQueryBuilder.append('}');
        if (!groupAttributes.isEmpty()) {
            newQueryBuilder.append(' ').append(buildGrouping());
        }
        if (!orderAttributes.isEmpty()) {
            newQueryBuilder.append(' ').append(buildOrdering());
        }
        sparql = newQueryBuilder.toString();
        LOG.trace("Translated SOQL query '{}' to SPARQL '{}'.", soql, sparql);
    }

    private StringBuilder getCountPart(String selectParameter) {
        StringBuilder countPart = new StringBuilder(" (COUNT(");
        if (isSelectedParamDistinct) {
            countPart.append(SoqlConstants.DISTINCT).append(' ');
        }
        countPart.append(selectParameter).append(") AS ?count) ");
        return countPart;
    }

    private StringBuilder processSupremeAttributes() {
        StringBuilder attributesPart = new StringBuilder();
        final Iterator<SoqlAttribute> it = attributes.iterator();
        while (it.hasNext()) {
            final SoqlAttribute current = it.next();
            if (current.isInstanceOf() || current.isOrderBy() || current.isGroupBy()) {
                processAttribute(current).forEach(attributesPart::append);
                it.remove();
            }
        }
        return attributesPart;
    }

    private StringBuilder processAttributes() {
        StringBuilder attributesPart = new StringBuilder();
        ArrayList<SoqlAttribute> toFilter = new ArrayList<>();
        ArrayList<SoqlAttribute> toInvFilter = new ArrayList<>();
        for (SoqlAttribute myAttr : attributes) {
            if (objectOfNextOr.contains(myAttr)) {
                StringBuilder orPart = new StringBuilder();
                orPart.append(processAllFilters(toFilter, toInvFilter));
                toFilter.clear();
                toInvFilter.clear();
                orPart.append("} UNION { ");
                attributesPart.append(orPart);
            }
            if (myAttr.isNot()) {
                toInvFilter.add(myAttr);
            } else {
                if (myAttr.requiresFilter()) {
                    toFilter.add(myAttr);
                }
                final List<String> bgps = processAttribute(myAttr);
                bgps.stream().filter(bgp -> attributesPart.indexOf(bgp) == -1).forEach(attributesPart::append);
            }
        }
        attributesPart.append(processAllFilters(toFilter, toInvFilter));
        return attributesPart;
    }

    private StringBuilder processAllFilters(ArrayList<SoqlAttribute> toFilter, ArrayList<SoqlAttribute> toInvFilter) {
        StringBuilder part = new StringBuilder();
        if (!toFilter.isEmpty()) {
            part.append(processFilter(toFilter));
        }
        if (!toInvFilter.isEmpty()) {
            part.append(processInvFilter(toInvFilter));
        }
        return part;
    }

    private String processFilter(ArrayList<SoqlAttribute> toFilter) {
        StringBuilder buildFilter = new StringBuilder();
        if (toFilter.isEmpty()) {
            return "";
        }
        buildFilter.append("FILTER (");
        buildFilter.append(toFilter.stream().map(SoqlAttribute::getFilterExpressions).flatMap(Collection::stream)
                                   .collect(Collectors.joining(" && ")));
        buildFilter.append(") ");
        return buildFilter.toString();
    }

    private String processInvFilter(ArrayList<SoqlAttribute> toInvFilter) {
        StringBuilder buildInvFilter = new StringBuilder();
        ArrayList<SoqlAttribute> toFilter = new ArrayList<>();
        if (toInvFilter.isEmpty()) {
            return "";
        }
        buildInvFilter.append("FILTER NOT EXISTS { ");
        for (SoqlAttribute attr : toInvFilter) {
            processAttribute(attr).forEach(buildInvFilter::append);
            if (attr.requiresFilter()) {
                toFilter.add(attr);
            }
        }
        buildInvFilter.append(processFilter(toFilter)).append("} ");
        return buildInvFilter.toString();
    }

    private List<String> processAttribute(SoqlAttribute attr) {
        return attr.getBasicGraphPattern(rootVariable, tpEnhancers);
    }

    private String buildOrdering() {
        StringBuilder sb = new StringBuilder(SoqlConstants.ORDER_BY);
        for (SoqlOrderParameter orderParam : orderAttributes) {
            sb.append(' ').append(orderParam.getOrderByPart());
        }
        return sb.toString();
    }

    private String buildGrouping() {
        StringBuilder sb = new StringBuilder(SoqlConstants.GROUP_BY);
        for (SoqlGroupParameter groupParam : groupAttributes) {
            sb.append(' ').append(groupParam.getGroupByPart());
        }
        return sb.toString();
    }
}
