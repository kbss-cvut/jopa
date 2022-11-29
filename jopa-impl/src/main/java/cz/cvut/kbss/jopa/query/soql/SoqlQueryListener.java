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
package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.HashMap;

public class SoqlQueryListener implements SoqlListener {

    private final MetamodelImpl metamodel;

    private String newQuery = "";

    private String typeDef = "SELECT";

    // keeps pointer at created object of SoqlAttribute while processing other neccessary rules
    private SoqlAttribute attrPointer;

    private final ArrayList<SoqlAttribute> attributes;

    // keeps index of first object of SoqlAttribute after OR operator
    private final ArrayList<SoqlAttribute> objectOfNextOr;

    private final ArrayList<SoqlOrderParameter> orderAttributes;

    private final ArrayList<SoqlGroupParameter> groupAttributes;

    private final HashMap<String, String> objectTypes;

    private boolean isSelectedParamDistinct = false;

    private boolean isSelectedParamCount = false;


    public SoqlQueryListener(MetamodelImpl metamodel) {
        super();
        this.metamodel = metamodel;
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
        buildString();
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
        SoqlAttribute myAttr = new SoqlAttribute();
        SoqlNode firstNode = new SoqlNode(getOwnerfromParam(ctx));
        SoqlNode actualNode = firstNode;
        for (int i = 2; i < ctx.getChildCount(); i += 2) {
            SoqlNode prevNode = actualNode;
            actualNode = new SoqlNode(prevNode, ctx.getChild(i).getText());
            prevNode.setChild(actualNode);
        }
        setIris(firstNode);
        myAttr.setFirstNode(firstNode);
        attributes.add(myAttr);
        attrPointer = myAttr;
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
        String owner = getOwnerfromParam(ctx);
        String attribute = getAttributefromParam(ctx);
        SoqlNode firstNode = new SoqlNode(owner);
        SoqlNode lastNode = new SoqlNode(firstNode, attribute);
        SoqlAttribute myAttr = new SoqlAttribute();
        firstNode.setChild(lastNode);
        setIris(firstNode);
        myAttr.setFirstNode(firstNode);
        attributes.add(myAttr);
        attrPointer = myAttr;
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
        if ("DISTINCT".equals(ctx.getChild(0).getText())) {
            isSelectedParamDistinct = true;
        }
    }

    @Override
    public void exitDistinct(SoqlParser.DistinctContext ctx) {
    }

    @Override
    public void enterLogOp(SoqlParser.LogOpContext ctx) {
    }

    @Override
    public void exitLogOp(SoqlParser.LogOpContext ctx) {
    }

    @Override
    public void enterWhereClauseWrapper(SoqlParser.WhereClauseWrapperContext ctx) {
    }

    @Override
    public void exitWhereClauseWrapper(SoqlParser.WhereClauseWrapperContext ctx) {
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
        SoqlAttribute myAttr = new SoqlAttribute();
        SoqlNode node = new SoqlNode(table);
        setObjectIri(node);
        myAttr.setFirstNode(node);
        myAttr.setOperator("");
        myAttr.setValue("");
        attributes.add(myAttr);
        attrPointer = myAttr;
    }

    @Override
    public void exitTableWithName(SoqlParser.TableWithNameContext ctx) {
    }

    @Override
    public void enterWhereClauses(SoqlParser.WhereClausesContext ctx) {
    }

    @Override
    public void exitWhereClauses(SoqlParser.WhereClausesContext ctx) {
    }

    @Override
    public void enterWhereClauseOps(SoqlParser.WhereClauseOpsContext ctx) {
    }

    @Override
    public void exitWhereClauseOps(SoqlParser.WhereClauseOpsContext ctx) {
    }

    @Override
    public void enterWhereClause(SoqlParser.WhereClauseContext ctx) {
    }

    @Override
    public void exitWhereClause(SoqlParser.WhereClauseContext ctx) {
        String logicalOperator = getOperators(ctx.getParent());
        String operator = ctx.getChild(1).getText();

        ParseTree whereClauseValue = ctx.getChild(2);

        attrPointer.setOperator(operator);
        attrPointer.setValue(whereClauseValue.getText());

        if ("OR".equals(logicalOperator)) {
            objectOfNextOr.add(attrPointer);
        }
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
        SoqlNode firstNode = new SoqlNode(getOwnerfromParam(ctx));
        SoqlNode actualNode = firstNode;
        for (int i = 2; i < ctx.getChildCount(); i += 2) {
            SoqlNode prevNode = actualNode;
            actualNode = new SoqlNode(prevNode, ctx.getChild(i).getText());
            prevNode.setChild(actualNode);
        }
        setIris(firstNode);
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
            SoqlAttribute myAttr = new SoqlAttribute();
            myAttr.setFirstNode(firstNode);
            myAttr.setOperator("");
            myAttr.setValue(orderParam.getAsValue());
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
        SoqlNode firstNode = new SoqlNode(getOwnerfromParam(ctx));
        SoqlNode actualNode = firstNode;
        for (int i = 2; i < ctx.getChildCount(); i += 2) {
            SoqlNode prevNode = actualNode;
            actualNode = new SoqlNode(prevNode, ctx.getChild(i).getText());
            prevNode.setChild(actualNode);
        }
        setIris(firstNode);
        SoqlGroupParameter groupParam = new SoqlGroupParameter(firstNode);
        boolean attrSet = false;
        for (SoqlAttribute attr : attributes) {
            if (attr.getAsParam().equals(groupParam.getAsParam())) {
                groupParam.setAttribute(attr);
                attrSet = true;
            }
        }
        if (!attrSet) {
            SoqlAttribute myAttr = new SoqlAttribute();
            myAttr.setFirstNode(firstNode);
            myAttr.setOperator("");
            myAttr.setValue(groupParam.getAsValue());
            myAttr.setGroupBy(true);
            attributes.add(1, myAttr);
            groupParam.setAttribute(myAttr);
        }
        groupAttributes.add(groupParam);
    }

    @Override
    public void exitGroupByParam(SoqlParser.GroupByParamContext ctx) {
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
    private String getOwnerfromParam(ParserRuleContext ctx) {
        return ctx.getChild(0).getChild(0).getText();
    }

    private String getAttributefromParam(ParserRuleContext ctx) {
        return ctx.getChild(2).getChild(0).getText();
    }

    private String getOperators(ParserRuleContext ctx) {
        String operator = "";
        switch (ctx.getChildCount()) {
            case 2:
                if (ctx.getChild(0).getChildCount() > 0) {
                    operator = ctx.getChild(0).getChild(0).getText();
                } else {
                    attrPointer.setNot(true);
                }
                break;
            case 3:
                attrPointer.setNot(true);
                operator = ctx.getChild(0).getChild(0).getText();
                break;
            default:
                attrPointer.setNot(false);
        }
        return operator;
    }

    private String getOrderingBy(ParserRuleContext ctx) {
        return ctx.getChildCount() > 1 ? ctx.getChild(1).getText() : "";
    }

    private void setObjectIri(SoqlNode node) {
        if (metamodel == null) {
            return;
        }
        IdentifiableEntityType<?> entityType = getEntityType(node.getValue());
        if (entityType == null) {
            return;
        }
        node.setIri(entityType.getIRI().toString());
        if (node.hasNextChild()) {
            setAllNodesIris(entityType, node.getChild());
        }
    }

    private IdentifiableEntityType<?> getEntityType(String name) {
        if (metamodel == null) {
            return null;
        }
        for (EntityType<?> type : metamodel.getEntities()) {
            IdentifiableEntityType<?> entityType = (IdentifiableEntityType<?>) type;
            if (entityType.getName().equals(name)) {
                return entityType;
            }
        }
        return null;
    }

    private IdentifiableEntityType<?> getEntityType(Type<?> type) {
        if (metamodel == null) {
            return null;
        }
        for (EntityType<?> value : metamodel.getEntities()) {
            IdentifiableEntityType<?> entityType = (IdentifiableEntityType<?>) value;
            if (entityType.equals(type)) {
                return entityType;
            }
        }
        return null;
    }

    private void setAllNodesIris(IdentifiableEntityType<?> entityType, SoqlNode node) {
        SingularAttributeImpl<?, ?> abstractAttribute = (SingularAttributeImpl<?, ?>) entityType.getAttribute(node.getValue());
        //not implemented case of 3 or more fragments (chained SoqlNodes)
        node.setIri(abstractAttribute.getIRI().toString());
        if (node.hasNextChild()) {
            Type<?> type = abstractAttribute.getType();
            IdentifiableEntityType<?> attrEntityType = getEntityType(type);
            if (attrEntityType == null) {
                return;
            }
            setAllNodesIris(attrEntityType, node.getChild());
        }
    }

    private void setIris(SoqlNode firstNode) {
        if (!objectTypes.containsKey(firstNode.getValue())) {
            return;
        }
        if (metamodel == null) {
            return;
        }
        String objectName = objectTypes.get(firstNode.getValue());
        IdentifiableEntityType<?> entityType = getEntityType(objectName);
        if (entityType == null) {
            return;
        }
        if (firstNode.hasNextChild()) {
            setAllNodesIris(entityType, firstNode.getChild());
        }
    }

    public String getSoqlQuery() {
        return newQuery;
    }


    //Methods to build new Query
    private void buildString() {
        if (attributes.isEmpty()) {
            return;
        }
        StringBuilder newQueryBuilder = new StringBuilder(typeDef);
        if (isSelectedParamCount) {
            newQueryBuilder.append(getCountPart());
        } else {
            if (isSelectedParamDistinct) {
                newQueryBuilder.append(" ").append("DISTINCT");
            }
            newQueryBuilder.append(" ?x ");
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
        newQueryBuilder.append("}");
        if (!groupAttributes.isEmpty()) {
            newQueryBuilder.append(" ").append(buildGrouping());
        }
        if (!orderAttributes.isEmpty()) {
            newQueryBuilder.append(" ").append(buildOrdering());
        }
        newQuery = newQueryBuilder.toString();
    }

    private StringBuilder getCountPart() {
        StringBuilder countPart = new StringBuilder(" (COUNT(");
        if (isSelectedParamDistinct) {
            countPart.append("distinct ");
        }
        countPart.append("?x) AS ?count) ");
        return countPart;
    }

    private StringBuilder processSupremeAttributes() {
        StringBuilder attributesPart = new StringBuilder();
        SoqlAttribute pointer = attributes.get(0);
        while (pointer.isObject() || pointer.isOrderBy() || pointer.isGroupBy()) {
            attributesPart.append(processAttribute(pointer));
            attributes.remove(pointer);
            if (attributes.isEmpty()) {
                break;
            } else {
                pointer = attributes.get(0);
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
                if (myAttr.isFilter()) {
                    toFilter.add(myAttr);
                }
                attributesPart.append(processAttribute(myAttr));
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

    private StringBuilder processFilter(ArrayList<SoqlAttribute> toFilter) {
        StringBuilder buildFilter = new StringBuilder();
        if (toFilter.isEmpty()) {
            return buildFilter;
        }
        buildFilter.append("FILTER (");
        for (SoqlAttribute attr : toFilter) {
            if (toFilter.indexOf(attr) != 0) {
                buildFilter.append(" && ");
            }
            buildFilter.append(attr.getFilter());
        }
        buildFilter.append(") ");
        return buildFilter;
    }

    private StringBuilder processInvFilter(ArrayList<SoqlAttribute> toInvFilter) {
        StringBuilder buildInvFilter = new StringBuilder();
        ArrayList<SoqlAttribute> toFilter = new ArrayList<>();
        if (toInvFilter.isEmpty()) {
            return buildInvFilter;
        }
        buildInvFilter.append("FILTER NOT EXISTS { ");
        for (SoqlAttribute attr : toInvFilter) {
            buildInvFilter.append(processAttribute(attr));
            if (attr.isFilter()) {
                toFilter.add(attr);
            }
        }
        buildInvFilter.append(processFilter(toFilter)).append("} ");
        return buildInvFilter;
    }

    private StringBuilder processAttribute(SoqlAttribute attr) {
        return new StringBuilder(attr.getTriplePattern());
    }

    private StringBuilder buildOrdering() {
        StringBuilder sb = new StringBuilder("ORDER BY");
        for (SoqlOrderParameter orderParam : orderAttributes) {
            sb.append(" ").append(orderParam.getOrderByPart());
        }
        return sb;
    }

    private StringBuilder buildGrouping() {
        StringBuilder sb = new StringBuilder("GROUP BY");
        for (SoqlGroupParameter groupParam : groupAttributes) {
            sb.append(" ").append(groupParam.getGroupByPart());
        }
        return sb;
    }
}
