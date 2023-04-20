/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.*;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

public class SoqlQueryListener implements SoqlListener {

    private static final Logger LOG = LoggerFactory.getLogger(SoqlQueryListener.class);

    private final MetamodelImpl metamodel;

    private String soql;
    private String sparql;

    private String typeDef = "SELECT";

    // keeps pointer at created object of SoqlAttribute while processing other necessary rules
    private SoqlAttribute attrPointer;

    private final ArrayList<SoqlAttribute> attributes;

    // keeps index of first object of SoqlAttribute after OR operator
    private final ArrayList<SoqlAttribute> objectOfNextOr;

    private final ArrayList<SoqlOrderParameter> orderAttributes;

    private final ArrayList<SoqlGroupParameter> groupAttributes;

    private final HashMap<String, String> objectTypes;

    private boolean isSelectedParamDistinct = false;

    private boolean isSelectedParamCount = false;

    private boolean isInObjectIdentifierExpression = false;

    private String rootVariable = "?x";


    public SoqlQueryListener(MetamodelImpl metamodel) {
        this.metamodel = Objects.requireNonNull(metamodel);
        this.attributes = new ArrayList<>();
        this.objectOfNextOr = new ArrayList<>();
        this.orderAttributes = new ArrayList<>();
        this.groupAttributes = new ArrayList<>();
        this.objectTypes = new HashMap<>();
    }

    @Override
    public void enterQuerySentence(SoqlParser.QuerySentenceContext ctx) {
    }

    @Override
    public void exitQuerySentence(SoqlParser.QuerySentenceContext ctx) {
        this.soql = ctx.getText();
        buildSparqlQueryString();
    }

    @Override
    public void enterSelectStatement(SoqlParser.SelectStatementContext ctx) {
    }

    @Override
    public void exitSelectStatement(SoqlParser.SelectStatementContext ctx) {
    }

    @Override
    public void enterParams(SoqlParser.ParamsContext ctx) {
    }

    @Override
    public void exitParams(SoqlParser.ParamsContext ctx) {
    }

    @Override
    public void enterParam(SoqlParser.ParamContext ctx) {
    }

    @Override
    public void exitParam(SoqlParser.ParamContext ctx) {
    }

    @Override
    public void enterJoinedParams(SoqlParser.JoinedParamsContext ctx) {
        SoqlNode firstNode = linkContextNodes(ctx);
        SoqlAttribute myAttr = new SoqlAttribute(firstNode);
        pushNewAttribute(myAttr);
    }

    private void pushNewAttribute(SoqlAttribute myAttr) {
        attributes.add(myAttr);
        this.attrPointer = myAttr;
    }

    @Override
    public void exitJoinedParams(SoqlParser.JoinedParamsContext ctx) {
    }

    @Override
    public void enterParamComma(SoqlParser.ParamCommaContext ctx) {
    }

    @Override
    public void exitParamComma(SoqlParser.ParamCommaContext ctx) {
    }

    @Override
    public void enterDistinctParam(SoqlParser.DistinctParamContext ctx) {
    }

    @Override
    public void exitDistinctParam(SoqlParser.DistinctParamContext ctx) {
    }

    @Override
    public void enterSelectedParam(SoqlParser.SelectedParamContext ctx) {
    }

    @Override
    public void exitSelectedParam(SoqlParser.SelectedParamContext ctx) {
    }

    @Override
    public void enterCount(SoqlParser.CountContext ctx) {
        isSelectedParamCount = true;
    }

    @Override
    public void exitCount(SoqlParser.CountContext ctx) {
    }

    @Override
    public void enterObject(SoqlParser.ObjectContext ctx) {
    }

    @Override
    public void exitObject(SoqlParser.ObjectContext ctx) {
    }

    @Override
    public void enterObjWithAttr(SoqlParser.ObjWithAttrContext ctx) {
        String owner = getOwnerFromParam(ctx);
        String attribute = getAttributeFromParam(ctx);
        // objectNode.attributeNode
        SoqlNode objectNode = new AttributeNode(owner);
        SoqlNode attributeNode = new AttributeNode(objectNode, attribute);
        objectNode.setChild(attributeNode);
        if (isIdentifier(objectNode, attributeNode)) {
            this.isInObjectIdentifierExpression = true;
        } else {
            setIris(objectNode);
            SoqlAttribute myAttr = new SoqlAttribute(objectNode);
            pushNewAttribute(myAttr);
        }
    }

    private boolean isIdentifier(SoqlNode objectNode, SoqlNode attributeNode) {
        if (!objectTypes.containsKey(objectNode.getValue())) {
            return false;
        }
        String objectName = objectTypes.get(objectNode.getValue());
        EntityTypeImpl<?> entityType = getEntityType(objectName);
        if (entityType == null) {
            return false;
        }
        return entityType.getIdentifier().getName().equals(attributeNode.getValue());
    }

    @Override
    public void exitObjWithAttr(SoqlParser.ObjWithAttrContext ctx) {
    }

    @Override
    public void enterObjWithOutAttr(SoqlParser.ObjWithOutAttrContext ctx) {
    }

    @Override
    public void exitObjWithOutAttr(SoqlParser.ObjWithOutAttrContext ctx) {
    }

    @Override
    public void enterAttribute(SoqlParser.AttributeContext ctx) {
    }

    @Override
    public void exitAttribute(SoqlParser.AttributeContext ctx) {
    }

    @Override
    public void enterTypeDef(SoqlParser.TypeDefContext ctx) {
        typeDef = ctx.getChild(0).getText();
    }

    @Override
    public void exitTypeDef(SoqlParser.TypeDefContext ctx) {
    }

    @Override
    public void enterDistinct(SoqlParser.DistinctContext ctx) {
        if (SoqlConstants.DISTINCT.equals(ctx.getChild(0).getText())) {
            isSelectedParamDistinct = true;
        }
    }

    @Override
    public void exitDistinct(SoqlParser.DistinctContext ctx) {
    }

    @Override
    public void enterWhereClauseWrapper(SoqlParser.WhereClauseWrapperContext ctx) {
    }

    @Override
    public void exitWhereClauseWrapper(SoqlParser.WhereClauseWrapperContext ctx) {
    }

    @Override
    public void enterConditionalExpression(SoqlParser.ConditionalExpressionContext ctx) {
    }

    @Override
    public void exitConditionalExpression(SoqlParser.ConditionalExpressionContext ctx) {
    }

    @Override
    public void enterConditionalTerm(SoqlParser.ConditionalTermContext ctx) {
    }

    @Override
    public void exitConditionalTerm(SoqlParser.ConditionalTermContext ctx) {
        final ParserRuleContext parentCtx = ctx.getParent();
        if (parentCtx.getChildCount() > 1 && !parentCtx.getChild(0).equals(ctx)) {
            objectOfNextOr.add(attrPointer);
        }
    }

    @Override
    public void enterConditionalFactor(SoqlParser.ConditionalFactorContext ctx) {
    }

    @Override
    public void exitConditionalFactor(SoqlParser.ConditionalFactorContext ctx) {
        if (ctx.getChildCount() > 1) {
            attrPointer.setNot(true);
        }
    }

    @Override
    public void enterSimpleConditionalExpression(SoqlParser.SimpleConditionalExpressionContext ctx) {
    }

    @Override
    public void exitSimpleConditionalExpression(SoqlParser.SimpleConditionalExpressionContext ctx) {
    }

    @Override
    public void enterInExpression(SoqlParser.InExpressionContext ctx) {
    }

    @Override
    public void exitInExpression(SoqlParser.InExpressionContext ctx) {
        assert ctx.getChildCount() > 2;
        if (isInObjectIdentifierExpression) {
            pushNewAttribute(createSyntheticAttributeForEntityId());
        }
        final ParseTree value = resolveInExpressionValue(ctx);
        if (ctx.getChild(1).getText().equals(SoqlConstants.NOT)) {
            attrPointer.setOperator(InOperator.notIn());
        } else {
            attrPointer.setOperator(InOperator.in());
        }
        attrPointer.setValue(value.getText());
        this.isInObjectIdentifierExpression = false;
    }

    private SoqlAttribute createSyntheticAttributeForEntityId() {
        return new SoqlAttribute(
                attrPointer.getFirstNode().hasChild() ? attrPointer.getFirstNode().getChild() :
                new AttributeNode(rootVariable.substring(1)));
    }

    private ParseTree resolveInExpressionValue(SoqlParser.InExpressionContext ctx) {
        final ParseTree lastToken = ctx.getChild(ctx.getChildCount() - 1);
        if (")".equals(lastToken.getText())) {
            return ctx.getChild(ctx.getChildCount() - 2);
        }
        return lastToken;
    }

    @Override
    public void enterInItem(SoqlParser.InItemContext ctx) {
    }

    @Override
    public void exitInItem(SoqlParser.InItemContext ctx) {
    }

    @Override
    public void enterLiteral(SoqlParser.LiteralContext ctx) {
    }

    @Override
    public void exitLiteral(SoqlParser.LiteralContext ctx) {
    }

    @Override
    public void enterLikeExpression(SoqlParser.LikeExpressionContext ctx) {
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
    public void enterComparisonExpression(SoqlParser.ComparisonExpressionContext ctx) {
    }

    @Override
    public void exitComparisonExpression(SoqlParser.ComparisonExpressionContext ctx) {
        String operator = ctx.getChild(1).getText();

        ParseTree whereClauseValue = ctx.getChild(2);

        if (isInObjectIdentifierExpression) {
            assert Objects.equals(operator, "=");
            if (attributes.size() == 1) {
                this.rootVariable = SoqlUtils.soqlVariableToSparqlVariable(whereClauseValue.getText());
            } else {
                final String varName = whereClauseValue.getText();
                attrPointer.getFirstNode().getChild().setValue(
                        varName.charAt(0) == SoqlConstants.VARIABLE_PREFIX ? varName.substring(1) : varName);
            }
        } else {
            attrPointer.setOperator(new ComparisonOperator(operator));
            attrPointer.setValue(whereClauseValue.getText());
        }
        this.isInObjectIdentifierExpression = false;
    }

    @Override
    public void enterTables(SoqlParser.TablesContext ctx) {
    }

    @Override
    public void exitTables(SoqlParser.TablesContext ctx) {
    }

    @Override
    public void enterTable(SoqlParser.TableContext ctx) {
    }

    @Override
    public void exitTable(SoqlParser.TableContext ctx) {
    }

    @Override
    public void enterTableName(SoqlParser.TableNameContext ctx) {
    }

    @Override
    public void exitTableName(SoqlParser.TableNameContext ctx) {
    }

    @Override
    public void enterTableWithName(SoqlParser.TableWithNameContext ctx) {
        String table = ctx.getChild(0).getChild(0).getText();
        String objectName = ctx.getChild(1).getChild(0).getText();
        objectTypes.put(objectName, table);
        SoqlNode node = new AttributeNode(table);
        setObjectIri(node);
        SoqlAttribute myAttr = new SoqlAttribute(node);
        pushNewAttribute(myAttr);
    }

    @Override
    public void exitTableWithName(SoqlParser.TableWithNameContext ctx) {
    }

    @Override
    public void enterWhereClauseValue(SoqlParser.WhereClauseValueContext ctx) {
    }

    @Override
    public void exitWhereClauseValue(SoqlParser.WhereClauseValueContext ctx) {
    }

    @Override
    public void enterWhereClauseParam(SoqlParser.WhereClauseParamContext ctx) {
    }

    @Override
    public void exitWhereClauseParam(SoqlParser.WhereClauseParamContext ctx) {
    }

    @Override
    public void enterStringExpression(SoqlParser.StringExpressionContext ctx) {

    }

    @Override
    public void exitStringExpression(SoqlParser.StringExpressionContext ctx) {

    }

    @Override
    public void enterFunctionsReturningStrings(SoqlParser.FunctionsReturningStringsContext ctx) {

    }

    @Override
    public void exitFunctionsReturningStrings(SoqlParser.FunctionsReturningStringsContext ctx) {
        final String functionName = ctx.getChild(0).getText();
        final FunctionNode node = new FunctionNode(attrPointer.getFirstNode(), functionName);
        attrPointer.setFirstNode(node);
    }

    @Override
    public void enterSimpleArithmeticExpression(SoqlParser.SimpleArithmeticExpressionContext ctx) {

    }

    @Override
    public void exitSimpleArithmeticExpression(SoqlParser.SimpleArithmeticExpressionContext ctx) {

    }

    @Override
    public void enterArithmeticTerm(SoqlParser.ArithmeticTermContext ctx) {

    }

    @Override
    public void exitArithmeticTerm(SoqlParser.ArithmeticTermContext ctx) {

    }

    @Override
    public void enterArithmeticFactor(SoqlParser.ArithmeticFactorContext ctx) {

    }

    @Override
    public void exitArithmeticFactor(SoqlParser.ArithmeticFactorContext ctx) {

    }

    @Override
    public void enterArithmeticPrimary(SoqlParser.ArithmeticPrimaryContext ctx) {

    }

    @Override
    public void exitArithmeticPrimary(SoqlParser.ArithmeticPrimaryContext ctx) {

    }

    @Override
    public void enterFunctionsReturningNumerics(SoqlParser.FunctionsReturningNumericsContext ctx) {
    }

    @Override
    public void exitFunctionsReturningNumerics(SoqlParser.FunctionsReturningNumericsContext ctx) {
        final String functionName = ctx.getChild(0).getText();
        final FunctionNode node = new FunctionNode(attrPointer.getFirstNode(), functionName);
        attrPointer.setFirstNode(node);
    }

    @Override
    public void enterOrderByClause(SoqlParser.OrderByClauseContext ctx) {
    }

    @Override
    public void exitOrderByClause(SoqlParser.OrderByClauseContext ctx) {
    }

    @Override
    public void enterOrderByFullFormComma(SoqlParser.OrderByFullFormCommaContext ctx) {
    }

    @Override
    public void exitOrderByFullFormComma(SoqlParser.OrderByFullFormCommaContext ctx) {
    }

    @Override
    public void enterOrderByFullForm(SoqlParser.OrderByFullFormContext ctx) {
    }

    @Override
    public void exitOrderByFullForm(SoqlParser.OrderByFullFormContext ctx) {
    }

    @Override
    public void enterOrderByParam(SoqlParser.OrderByParamContext ctx) {
        SoqlNode firstNode = linkContextNodes(ctx);
        String orderingBy = getOrderingBy(ctx.getParent());
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
    public void exitOrderByParam(SoqlParser.OrderByParamContext ctx) {
    }

    @Override
    public void enterGroupByClause(SoqlParser.GroupByClauseContext ctx) {
    }

    @Override
    public void exitGroupByClause(SoqlParser.GroupByClauseContext ctx) {
    }

    @Override
    public void enterGroupByParamComma(SoqlParser.GroupByParamCommaContext ctx) {
    }

    @Override
    public void exitGroupByParamComma(SoqlParser.GroupByParamCommaContext ctx) {
    }

    @Override
    public void enterGroupByParam(SoqlParser.GroupByParamContext ctx) {
        SoqlNode firstNode = linkContextNodes(ctx);
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

    private SoqlNode linkContextNodes(ParserRuleContext ctx) {
        SoqlNode firstNode = new AttributeNode(getOwnerFromParam(ctx));
        SoqlNode currentNode = firstNode;
        for (int i = 2; i < ctx.getChildCount(); i += 2) {
            SoqlNode prevNode = currentNode;
            currentNode = new AttributeNode(prevNode, ctx.getChild(i).getText());
            prevNode.setChild(currentNode);
        }
        setIris(firstNode);
        if (currentNode.getIri().isEmpty()) {
            currentNode.getParent().setChild(null);
            this.isInObjectIdentifierExpression = true;
        }
        return firstNode;
    }

    @Override
    public void exitGroupByParam(SoqlParser.GroupByParamContext ctx) {
    }

    @Override
    public void enterInputParameter(SoqlParser.InputParameterContext ctx) {

    }

    @Override
    public void exitInputParameter(SoqlParser.InputParameterContext ctx) {

    }

    @Override
    public void visitTerminal(TerminalNode terminalNode) {
    }

    @Override
    public void visitErrorNode(ErrorNode errorNode) {
    }

    @Override
    public void enterEveryRule(ParserRuleContext parserRuleContext) {
    }

    @Override
    public void exitEveryRule(ParserRuleContext parserRuleContext) {
    }

    //Methods to help parse tree
    private String getOwnerFromParam(ParserRuleContext ctx) {
        return ctx.getChild(0).getChild(0).getText();
    }

    private String getAttributeFromParam(ParserRuleContext ctx) {
        return ctx.getChild(2).getChild(0).getText();
    }

    private String getOrderingBy(ParserRuleContext ctx) {
        return ctx.getChildCount() > 1 ? ctx.getChild(1).getText() : "";
    }

    private void setObjectIri(SoqlNode node) {
        EntityTypeImpl<?> entityType = getEntityType(node.getValue());
        if (entityType == null) {
            return;
        }
        node.setIri(entityType.getIRI().toString());
        if (node.hasChild()) {
            setAllNodesIris(entityType, node.getChild());
        }
    }

    private EntityTypeImpl<?> getEntityType(String name) {
        for (EntityType<?> type : metamodel.getEntities()) {
            EntityTypeImpl<?> entityType = (EntityTypeImpl<?>) type;
            if (entityType.getName().equals(name)) {
                return entityType;
            }
        }
        return null;
    }

    private void setAllNodesIris(EntityType<?> entityType, SoqlNode node) {
        if (entityType.getIdentifier().getName().equals(node.getValue())) {
            return;
        }
        final Attribute<?, ?> abstractAttribute = entityType.getAttribute(node.getValue());
        //not implemented case of 3 or more fragments (chained SoqlNodes)
        node.setIri(abstractAttribute.getIRI().toString());
        if (node.hasChild()) {
            final Type<?> type = resolveBindableType(abstractAttribute);
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
        EntityTypeImpl<?> entityType = getEntityType(objectName);
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

    //Methods to build new Query
    private void buildSparqlQueryString() {
        if (attributes.isEmpty()) {
            return;
        }
        StringBuilder newQueryBuilder = new StringBuilder(typeDef);
        if (isSelectedParamCount) {
            newQueryBuilder.append(getCountPart());
        } else {
            if (isSelectedParamDistinct) {
                newQueryBuilder.append(' ').append(SoqlConstants.DISTINCT);
            }
            newQueryBuilder.append(' ').append(rootVariable).append(' ');
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

    private StringBuilder getCountPart() {
        StringBuilder countPart = new StringBuilder(" (COUNT(");
        if (isSelectedParamDistinct) {
            countPart.append(SoqlConstants.DISTINCT).append(' ');
        }
        countPart.append(rootVariable).append(") AS ?count) ");
        return countPart;
    }

    private StringBuilder processSupremeAttributes() {
        StringBuilder attributesPart = new StringBuilder();
        final Iterator<SoqlAttribute> it = attributes.iterator();
        while (it.hasNext()) {
            final SoqlAttribute current = it.next();
            if (current.isInstanceOf() || current.isOrderBy() || current.isGroupBy()) {
                attributesPart.append(processAttribute(current));
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
                final String bgp = processAttribute(myAttr);
                if (attributesPart.indexOf(bgp) == -1) {
                    attributesPart.append(bgp);
                }
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
            buildInvFilter.append(processAttribute(attr));
            if (attr.requiresFilter()) {
                toFilter.add(attr);
            }
        }
        buildInvFilter.append(processFilter(toFilter)).append("} ");
        return buildInvFilter.toString();
    }

    private String processAttribute(SoqlAttribute attr) {
        return attr.getBasicGraphPattern(rootVariable);
    }

    private String buildOrdering() {
        StringBuilder sb = new StringBuilder("ORDER BY");
        for (SoqlOrderParameter orderParam : orderAttributes) {
            sb.append(' ').append(orderParam.getOrderByPart());
        }
        return sb.toString();
    }

    private String buildGrouping() {
        StringBuilder sb = new StringBuilder("GROUP BY");
        for (SoqlGroupParameter groupParam : groupAttributes) {
            sb.append(' ').append(groupParam.getGroupByPart());
        }
        return sb.toString();
    }
}
