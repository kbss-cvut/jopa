package cz.cvut.kbss.jopa.query.soql;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class SoqlQueryListener implements soqlListener {

    private MetamodelImpl metamodel;

    private String newQuery = "";

    private String typeDef = "SELECT";

    // keeps pointer at created object of SoqlAttribute while processing other neccessary rules
    private SoqlAttribute attrPointer;

    private ArrayList<SoqlAttribute> attributes;

    // keeps index of first object of SoqlAttribute after OR operator
    private ArrayList<SoqlAttribute> objectOfNextOr;

    private ArrayList<SoqlOrderParam> orderAttributes;

    private ArrayList<SoqlGroupParam> groupAttributes;

    private HashMap<String, String> objectTypes;

    private boolean isSelectedParamDistinct = false;



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
    public void enterQuerySentence(soqlParser.QuerySentenceContext ctx) {}

    @Override
    public void exitQuerySentence(soqlParser.QuerySentenceContext ctx) { buildString(); }

    @Override
    public void enterSelectStatement(soqlParser.SelectStatementContext ctx) {}

    @Override
    public void exitSelectStatement(soqlParser.SelectStatementContext ctx) {}

    @Override
    public void enterParams(soqlParser.ParamsContext ctx) {}

    @Override
    public void exitParams(soqlParser.ParamsContext ctx) {}

    @Override
    public void enterParam(soqlParser.ParamContext ctx) {}

    @Override
    public void exitParam(soqlParser.ParamContext ctx) {}

    @Override
    public void enterJoinedParams(soqlParser.JoinedParamsContext ctx) {
        SoqlAttribute myAttr = new SoqlAttribute();
        SoqlNode firstNode = new SoqlNode(getOwnerfromParam(ctx));
        SoqlNode actualNode = firstNode;
        for(int i = 2; i < ctx.getChildCount(); i += 2){
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
    public void exitJoinedParams(soqlParser.JoinedParamsContext ctx) {}

    @Override
    public void enterParamComma(soqlParser.ParamCommaContext ctx) {}

    @Override
    public void exitParamComma(soqlParser.ParamCommaContext ctx) {}

    @Override
    public void enterDistinctParam(soqlParser.DistinctParamContext ctx) {}

    @Override
    public void exitDistinctParam(soqlParser.DistinctParamContext ctx) {}

    @Override
    public void enterObject(soqlParser.ObjectContext ctx) {}

    @Override
    public void exitObject(soqlParser.ObjectContext ctx) {}

    @Override
    public void enterObjWithAttr(soqlParser.ObjWithAttrContext ctx) {
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
    public void exitObjWithAttr(soqlParser.ObjWithAttrContext ctx) {}

    @Override
    public void enterObjWithOutAttr(soqlParser.ObjWithOutAttrContext ctx) { }

    @Override
    public void exitObjWithOutAttr(soqlParser.ObjWithOutAttrContext ctx) {}

    @Override
    public void enterAttribute(soqlParser.AttributeContext ctx) {}

    @Override
    public void exitAttribute(soqlParser.AttributeContext ctx) {}

    @Override
    public void enterTypeDef(soqlParser.TypeDefContext ctx) { typeDef = ctx.getChild(0).getText(); }

    @Override
    public void exitTypeDef(soqlParser.TypeDefContext ctx) {}

    @Override
    public void enterDistinct(soqlParser.DistinctContext ctx) {
        if(ctx.getChild(0).getText().equals("DISTINCT")){
            isSelectedParamDistinct = true;
        }
    }

    @Override
    public void exitDistinct(soqlParser.DistinctContext ctx) {}

    @Override
    public void enterLogOp(soqlParser.LogOpContext ctx) {}

    @Override
    public void exitLogOp(soqlParser.LogOpContext ctx) {}

    @Override
    public void enterWhereClausuleWrapper(soqlParser.WhereClausuleWrapperContext ctx) {}

    @Override
    public void exitWhereClausuleWrapper(soqlParser.WhereClausuleWrapperContext ctx) {}

    @Override
    public void enterTables(soqlParser.TablesContext ctx) {}

    @Override
    public void exitTables(soqlParser.TablesContext ctx) {}

    @Override
    public void enterTable(soqlParser.TableContext ctx) {}

    @Override
    public void exitTable(soqlParser.TableContext ctx) {}

    @Override
    public void enterTableName(soqlParser.TableNameContext ctx) {}

    @Override
    public void exitTableName(soqlParser.TableNameContext ctx) {}

    @Override
    public void enterTableWithName(soqlParser.TableWithNameContext ctx) {
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
    public void exitTableWithName(soqlParser.TableWithNameContext ctx) {}

    @Override
    public void enterWhereClausules(soqlParser.WhereClausulesContext ctx) {}

    @Override
    public void exitWhereClausules(soqlParser.WhereClausulesContext ctx) {}

    @Override
    public void enterWhereClausuleOps(soqlParser.WhereClausuleOpsContext ctx) {}

    @Override
    public void exitWhereClausuleOps(soqlParser.WhereClausuleOpsContext ctx) {}

    @Override
    public void enterWhereClausule(soqlParser.WhereClausuleContext ctx) {}

    @Override
    public void exitWhereClausule(soqlParser.WhereClausuleContext ctx) {
        String logicalOperator = getOperators(ctx.getParent());
        String operator = ctx.getChild(1).getText();

        ParseTree whereClausuleValue = ctx.getChild(2);

        attrPointer.setOperator(operator);
        attrPointer.setValue(whereClausuleValue.getText());

        if (logicalOperator.equals("OR")){
            objectOfNextOr.add(attrPointer);
        }
    }

    @Override
    public void enterWhereClausuleValue(soqlParser.WhereClausuleValueContext ctx) {}

    @Override
    public void exitWhereClausuleValue(soqlParser.WhereClausuleValueContext ctx) {}

    @Override
    public void enterWhereClausuleParam(soqlParser.WhereClausuleParamContext ctx) {}

    @Override
    public void exitWhereClausuleParam(soqlParser.WhereClausuleParamContext ctx) {}

    @Override
    public void enterOrderByClausule(soqlParser.OrderByClausuleContext ctx) {}

    @Override
    public void exitOrderByClausule(soqlParser.OrderByClausuleContext ctx) {}

    @Override
    public void enterOrderByFullFormComma(soqlParser.OrderByFullFormCommaContext ctx) {}

    @Override
    public void exitOrderByFullFormComma(soqlParser.OrderByFullFormCommaContext ctx) {}

    @Override
    public void enterOrderByFullForm(soqlParser.OrderByFullFormContext ctx) {}

    @Override
    public void exitOrderByFullForm(soqlParser.OrderByFullFormContext ctx) {}

    @Override
    public void enterOrderByParam(soqlParser.OrderByParamContext ctx) {
        SoqlNode firstNode = new SoqlNode(getOwnerfromParam(ctx));
        SoqlNode actualNode = firstNode;
        for(int i = 2; i < ctx.getChildCount(); i += 2){
            SoqlNode prevNode = actualNode;
            actualNode = new SoqlNode(prevNode, ctx.getChild(i).getText());
            prevNode.setChild(actualNode);
        }
        setIris(firstNode);
        String orderingBy = getOrderingBy(ctx.getParent());
        SoqlOrderParam orderParam = new SoqlOrderParam(firstNode, orderingBy);
        boolean attrSet = false;
        for (SoqlAttribute attr: attributes) {
            if (attr.getAsParam().equals(orderParam.getAsParam())){
                orderParam.setAttribute(attr);
                attrSet = true;
            }
        }
        if(!attrSet){
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
    public void exitOrderByParam(soqlParser.OrderByParamContext ctx) {}

    @Override
    public void enterGroupByClausule(soqlParser.GroupByClausuleContext ctx) {}

    @Override
    public void exitGroupByClausule(soqlParser.GroupByClausuleContext ctx) {}

    @Override
    public void enterGroupByParamComma(soqlParser.GroupByParamCommaContext ctx) {}

    @Override
    public void exitGroupByParamComma(soqlParser.GroupByParamCommaContext ctx) {}

    @Override
    public void enterGroupByParam(soqlParser.GroupByParamContext ctx) {
        SoqlNode firstNode = new SoqlNode(getOwnerfromParam(ctx));
        SoqlNode actualNode = firstNode;
        for(int i = 2; i < ctx.getChildCount(); i += 2){
            SoqlNode prevNode = actualNode;
            actualNode = new SoqlNode(prevNode, ctx.getChild(i).getText());
            prevNode.setChild(actualNode);
        }
        setIris(firstNode);
        SoqlGroupParam groupParam = new SoqlGroupParam(firstNode);
        boolean attrSet = false;
        for (SoqlAttribute attr: attributes) {
            if (attr.getAsParam().equals(groupParam.getAsParam())){
                groupParam.setAttribute(attr);
                attrSet = true;
            }
        }
        if(!attrSet){
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
    public void exitGroupByParam(soqlParser.GroupByParamContext ctx) {}

    @Override
    public void visitTerminal(TerminalNode terminalNode) {}

    @Override
    public void visitErrorNode(ErrorNode errorNode) {}

    @Override
    public void enterEveryRule(ParserRuleContext parserRuleContext) {}

    @Override
    public void exitEveryRule(ParserRuleContext parserRuleContext) {}

    //Methods to help parse tree
    private String getOwnerfromParam(ParserRuleContext ctx){ return ctx.getChild(0).getChild(0).getText(); }

    private String getAttributefromParam(ParserRuleContext ctx){ return ctx.getChild(2).getChild(0).getText(); }

    private String getOperators(ParserRuleContext ctx){
        String operator = "";
        switch (ctx.getChildCount()){
            case 1:
                attrPointer.setNot(false);
                break;
            case 2:
                if(ctx.getChild(0).getChildCount() > 0){
                    operator = ctx.getChild(0).getChild(0).getText();
                }else{
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

    private String getOrderingBy(ParserRuleContext ctx){ return ctx.getChildCount() > 1 ? ctx.getChild(1).getText() : ""; }

    private void setObjectIri(SoqlNode node){
        if(metamodel == null){
            return;
        }
        EntityTypeImpl<?> entityType = getEntityType(node.getValue());
        if(entityType == null){
            return;
        }
        node.setIri(entityType.getIRI().toString());
        if(node.hasNextChild()){
            setAllNodesIris(entityType, node.getChild());
        }
    }

    private EntityTypeImpl<?> getEntityType(String name){
        if(metamodel == null){
            return null;
        }
        Iterator<EntityType<?>> iterator = metamodel.getEntities().iterator();
        while(iterator.hasNext()){
            EntityTypeImpl<?> entityType = (EntityTypeImpl<?>) iterator.next();
            if(entityType.getName().equals(name)){
                return entityType;
            }
        }
        return null;
    }

    private void setAllNodesIris(EntityTypeImpl<?> entityType, SoqlNode node){
        AbstractAttribute abstractAttribute = entityType.getAttribute(node.getValue());
        //not implemented case of 3 or more fragments (chained SoqlNodes)
//        if(node.hasNextChild()){
        if(false) {
            // from abstractAttribute get name of Java Class of abstractAttribute
            String className = "";
            EntityTypeImpl<?> attrEntityType = getEntityType(className);
            if(attrEntityType == null){
                return;
            }
            setAllNodesIris(attrEntityType, node.getChild());
        }else{
            node.setIri(abstractAttribute.getIRI().toString());
        }
    }

    private void setIris(SoqlNode firstNode){
        if(!objectTypes.containsKey(firstNode.getValue())){
            return;
        }
        if(metamodel == null){
            return;
        }
        String objectName = objectTypes.get(firstNode.getValue());
        EntityTypeImpl<?> entityType = getEntityType(objectName);
        if(entityType == null){
            return;
        }
        if(firstNode.hasNextChild()){
            setAllNodesIris(entityType, firstNode.getChild());
        }
    }

    public String getSoqlQuery(){ return newQuery; }


    //Methods to build new Query
    private void buildString(){
        if(attributes.isEmpty()){
            return;
        }
        StringBuilder newQueryBuilder = new StringBuilder(typeDef);
        if(isSelectedParamDistinct){
            newQueryBuilder.append(" ").append("DISTINCT");
        }
        newQueryBuilder.append(" ?x WHERE { ");
        newQueryBuilder.append(processSupremeAttributes());
        if(!objectOfNextOr.isEmpty()){
            newQueryBuilder.append("{ ");
        }
        newQueryBuilder.append(processAttributes());
        if(!objectOfNextOr.isEmpty()){
            newQueryBuilder.append("} ");
        }
        newQueryBuilder.append("}");
        if(!groupAttributes.isEmpty()){
            newQueryBuilder.append(" ").append(buildGrouping());
        }
        if(!orderAttributes.isEmpty()){
            newQueryBuilder.append(" ").append(buildOrdering());
        }
        newQuery = newQueryBuilder.toString();
    }

    private StringBuilder processSupremeAttributes(){
        StringBuilder attributesPart = new StringBuilder();
        SoqlAttribute pointer = attributes.get(0);
        while(pointer.isObject() || pointer.isOrderBy() || pointer.isGroupBy()){
            attributesPart.append(processAttribute(pointer));
            attributes.remove(pointer);
            if(attributes.isEmpty()){
                break;
            }else{
                pointer = attributes.get(0);
            }
        }
        return attributesPart;
    }

    private StringBuilder processAttributes(){
        StringBuilder attributesPart = new StringBuilder();
        ArrayList<SoqlAttribute> toFilter = new ArrayList<>();
        ArrayList<SoqlAttribute> toInvFilter = new ArrayList<>();
        for (SoqlAttribute myAttr: attributes) {
            if(objectOfNextOr.contains(myAttr)){
                StringBuilder orPart = new StringBuilder();
                orPart.append(processAllFilters(toFilter,toInvFilter));
                toFilter.clear();
                toInvFilter.clear();
                orPart.append("} UNION { ");
                attributesPart.append(orPart);
            }
            if(myAttr.isNot()){
                toInvFilter.add(myAttr);
            }else{
                if (myAttr.isFilter()){
                    toFilter.add(myAttr);
                }
                attributesPart.append(processAttribute(myAttr));
            }
        }
        attributesPart.append(processAllFilters(toFilter,toInvFilter));
        return attributesPart;
    }

    private StringBuilder processAllFilters(ArrayList<SoqlAttribute> toFilter, ArrayList<SoqlAttribute> toInvFilter){
        StringBuilder part = new StringBuilder();
        if(!toFilter.isEmpty()){
            part.append(processFilter(toFilter));
        }
        if(!toInvFilter.isEmpty()){
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
        buildInvFilter.append("FILTER NOT EXISTS ( ");
        for (SoqlAttribute attr : toInvFilter) {
            buildInvFilter.append(processAttribute(attr));
            if(attr.isFilter()){
                toFilter.add(attr);
            }
        }
        buildInvFilter.append(processFilter(toFilter)).append(") ");
        return buildInvFilter;
    }

    private StringBuilder processAttribute(SoqlAttribute attr) { return new StringBuilder(attr.getTripplePattern()); }

    private StringBuilder buildOrdering(){
        StringBuilder sb = new StringBuilder("ORDER BY");
        for (SoqlOrderParam orderParam: orderAttributes) {
            sb.append(" ").append(orderParam.getOrderByPart());
        }
        return sb;
    }

    private StringBuilder buildGrouping(){
        StringBuilder sb = new StringBuilder("GROUP BY");
        for (SoqlGroupParam groupParam: groupAttributes) {
            sb.append(" ").append(groupParam.getGroupByPart());
        }
        return sb;
    }
}
