package cz.cvut.kbss.jopa.query.soql;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;

public class SoqlQueryListener implements soqlListener {

    private String newQuery;
    private String prefix = "http://www.example.org/";

    private String rdfType = "rdf:type";

    private String typeDef;

    private SoqlAttribute attrPointer;

    private ArrayList<SoqlAttribute> attributes;

    private ArrayList<Integer> indexOfNextOr;

    private ArrayList<SoqlOrderParam> orderAttributes;

    private ArrayList<SoqlGroupParam> groupAttributes;

    private boolean isSelectedParamDistinct = false;



    public SoqlQueryListener() {
        super();
        this.newQuery = "";
        this.typeDef = "SELECT";
        this.attributes = new ArrayList<>();
        this.indexOfNextOr = new ArrayList<>();
        this.orderAttributes = new ArrayList<>();
        this.groupAttributes = new ArrayList<>();
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
        myAttr.setFirstNode(firstNode);
        myAttr.setPrefix(prefix);
        myAttr.setRdfType(rdfType);
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
        myAttr.setFirstNode(firstNode);
        myAttr.setPrefix(prefix);
        myAttr.setRdfType(rdfType);
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
        SoqlAttribute myAttr = new SoqlAttribute();
        SoqlNode node = new SoqlNode(table);
        myAttr.setFirstNode(node);
        myAttr.setPrefix(prefix);
        myAttr.setRdfType(rdfType);
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
            indexOfNextOr.add(attributes.indexOf(attrPointer));
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
        String orderingBy = getOrderingBy(ctx.getParent());
        SoqlOrderParam orderParam = new SoqlOrderParam(firstNode, orderingBy);
        for (SoqlAttribute attr: attributes) {
            if (attr.getAsParam().equals(orderParam.getAsParam())){
                orderParam.setAttribute(attr);
            }
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
        SoqlGroupParam groupParam = new SoqlGroupParam(firstNode);
        for (SoqlAttribute attr: attributes) {
            if (attr.getAsParam().equals(groupParam.getAsParam())){
                groupParam.setAttribute(attr);
            }
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

    public String getSoqlQuery(){
        return newQuery;
    }


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
        if(!indexOfNextOr.isEmpty()){
            newQueryBuilder.append("{ ");
        }
        newQueryBuilder.append(processAttributes());
        if(!indexOfNextOr.isEmpty()){
            newQueryBuilder.append(" } ");
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

    private StringBuilder processAttributes(){
        StringBuilder attributesPart = new StringBuilder();
        ArrayList<SoqlAttribute> toFilter = new ArrayList<>();
        ArrayList<SoqlAttribute> toInvFilter = new ArrayList<>();
        for (SoqlAttribute myAttr: attributes) {
            if(indexOfNextOr.contains(attributes.indexOf(myAttr))){
                StringBuilder orPart = new StringBuilder();
                orPart.append(processAllFilters(toFilter,toInvFilter));
                toFilter.clear();
                toInvFilter.clear();
                orPart.append(" } UNION { ");
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
        buildInvFilter.append(processFilter(toFilter)).append(")");
        return buildInvFilter;
    }

    private StringBuilder processAttribute(SoqlAttribute attr) {
        return new StringBuilder(attr.getTripplePattern());
    }

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
